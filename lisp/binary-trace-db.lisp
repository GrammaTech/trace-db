;; binary-trace-db.lisp - Trace database which uses a proprietary trace
;; format and C backend.

(in-package :trace-db)
(in-readtable :curry-compose-reader-macros)

(define-constant +lib-dir+
    (make-pathname :directory (butlast (pathname-directory
                                        #.(or *compile-file-truename*
                                              *load-truename*
                                              *default-pathname-defaults*))))
  :test #'equalp
  :documentation "Path to directory holding shared library.")

(defvar *trace-db-loaded* nil)
(defun load-libtrace-db ()
  (unless *trace-db-loaded*
    (pushnew +lib-dir+ *foreign-library-directories*
             :test #'equal)

    (define-foreign-library trace-db
      (t (:default "libtrace-db")))

    (use-foreign-library trace-db)

    (setf *trace-db-loaded* t)))

(defcenum type-format
  :unsigned
  :signed
  :float
  :pointer
  :blob)

(defcstruct (buffer-size :class c-buffer-size)
  (address :uint64)
  (size :uint64))

(defcstruct (type-description :class c-type-description)
  (name-index :uint32)
  (format type-format)
  (size :uint32))

(defstruct (type-description (:conc-name type-))
  (name-index nil :type fixnum)
  (format nil :type symbol)
  (size nil :type (unsigned-byte 32)))

(defcstruct (var-info :class c-var-info)
  ;; CFFI's handling of the union here seems to be broken.
  ;; Use an integer here to get the correct layout, and figure out the
  ;; real type when we dereference.
  (value :uint64)
  (name-index :uint32)
  (type-index :uint32)
  (size :uint32)
  (buffer-size :uint64)
  (has-buffer-size :uint8))

(defcstruct trace-read-state
  (file :pointer)
  (names (:pointer :string))
  (n-names :uint32)
  (types (:pointer (:struct type-description)))
  (n-types :uint32))

(defcstruct (trace-point :class c-trace-point)
  (statement :uint64)
  (sizes (:pointer (:struct buffer-size)))
  (n-sizes :uint32)
  (vars (:pointer (:struct var-info)))
  (n-vars :uint32)
  (aux (:pointer :uint64))
  (n-aux :uint32))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod expand-from-foreign (info (type c-buffer-size))
    `(with-foreign-slots ((address size) ,info (:struct buffer-size))
       (cons address size)))
  (defmethod expand-from-foreign (info (type c-var-info))
    `(with-foreign-slots ((name-index type-index size
                           has-buffer-size buffer-size)
                          ,info
                          (:struct var-info))
       `(,name-index ,type-index ,size ,(unless (zerop has-buffer-size)
                                                buffer-size))))
  (defmethod expand-from-foreign (point (type c-trace-point))
    `(with-foreign-slots ((statement sizes n-sizes vars n-vars aux n-aux)
                          ,point (:struct trace-point))
       (list statement sizes n-sizes vars n-vars aux n-aux)))
  (defmethod expand-from-foreign (type-struct (type c-type-description))
    `(with-foreign-slots ((name-index format size)
                          ,type-struct (:struct type-description))
       (make-type-description :name-index name-index
                              :format format
                              :size size))))

(defcfun start-reading (:pointer (:struct trace-read-state))
  (filename :string)
  (timeout :int))

(defcfun end-reading :void
  (state :pointer))

(defcfun read-trace-point :int
  (state :pointer) (results :pointer))

(defconstant +trace-id-file-bits+ 23
  "Number of bits in trace ID used to identify the file.")
(defconstant +trace-id-statement-bits+ 40
  "Number of bits in trace ID used to identify the statement within a file.")

(defun convert-trace-point (names types point &optional (index 0) &aux result)
  "Convert a trace point to an alist."
  (labels
      ((get-value (vars type size index)
         (let* ((offset (* index (foreign-type-size '(:struct var-info))))
                (data (mem-ref vars
                               (ecase (type-format type)
                                 (:unsigned :uint64)
                                 (:signed :int64)
                                 (:float
                                  (if (= 4 (type-size type)) :float :double))
                                 (:pointer :uint64)
                                 (:blob :pointer))
                               offset)))
           (if (eq :blob (type-format type))
               ;; Blob: convert to string, or collect into byte array.
               (prog1
                   (restart-case
                       (foreign-string-to-lisp data :count size)
                     (store-as-byte-array ()
                       :report "Store as a byte array"
                       (iter (for i below size)
                         (collecting (mem-ref data :char i)
                           result-type 'vector))))
                 (foreign-free data))

             ;; Primitive type
             data)))
       (read-vars (vars count)
         (iter (for i below count)
               (destructuring-bind (name-index type-index size buffer-size)
                   (mem-aref vars '(:struct var-info) i)
                 (let ((type (aref types type-index)))
                   (collect `#(,(aref names name-index)
                               ,(aref names (type-name-index type))
                               ,(get-value vars type size i)
                               ,buffer-size)))))))
    (destructuring-bind (statement sizes n-sizes vars n-vars aux n-aux)
        (mem-aref point '(:struct trace-point) index)
      (declare (ignorable sizes n-sizes))
      (when (> n-vars 0)
        (push (cons :scopes (read-vars vars n-vars))
              result))
      (when (> n-aux 0)
        (push (cons :aux
                    (foreign-array-to-lisp aux `(:array :uint64 ,n-aux)))
              result))
      (when (> statement 0)
        ;; SEL clang-project instrumentation packs file and AST
        ;; indices into the trace ID, with the top bits used as a flag
        ;; to indicate the presence of the file ID.
        ;; Since SEL is likely to be the only Lisp client, it's
        ;; convenient to handle this here.
        (if (> (logand statement (ash 1 63)) 0)
            (progn (push (cons :c (logand (1- (ash 1 +trace-id-statement-bits+))
                                          statement))
                         result)
                   (push (cons :f
                               (logand (1- (ash 1 +trace-id-file-bits+))
                                       (ash statement
                                            (* -1 +trace-id-statement-bits+))))
                         result))
         (push (cons :c statement) result)))))
  result)

(defun read-binary-trace (file timeout &key (predicate #'identity) max
                          &aux (collected 0))
  "Read a trace and convert to a list."
  (load-libtrace-db)
  (let ((state-ptr (start-reading file timeout)))
    (unless (null-pointer-p state-ptr)
      (let* ((state (mem-aref state-ptr '(:struct trace-read-state)))
             (names (ignore-errors
                      (iter (with ptr = (getf state 'names))
                            (for i below (getf state 'n-names))
                            (for str = (mem-aref ptr :string i))
                            (while str)
                            (collect str result-type 'vector))))
             (types (ignore-errors
                      (iter (with ptr = (getf state 'types))
                            (for i below (getf state 'n-types))
                            (for str =
                                 (mem-aref ptr '(:struct type-description) i))
                            (while str)
                            (collect str result-type 'vector)))))
        (unwind-protect
             (when (and types names)
               (with-foreign-object (point-struct '(:struct trace-point))
                 (iter (while
                           (and (eq 0 (read-trace-point state-ptr point-struct))
                                (or (null max) (< collected max))))
                       (for trace-point =
                            (convert-trace-point names types point-struct))
                       (when (funcall predicate trace-point)
                         (incf collected)
                         (collect trace-point)))))
          (end-reading state-ptr))))))



;;; Trace DB interface
(defcfun create-db :pointer)
(defcfun ("add_trace" c-add-trace) :void
  (db :pointer) (state :pointer) (max :uint64))
(defcfun free-db :void (db :pointer))

(defcstruct c-trace
  (points (:pointer (:struct trace-point)))
  (n-points :uint64)
  (n-points-allocated :uint64)

  (names (:pointer :string))
  (n-names :uint32)
  (types (:pointer (:struct type-description)))
  (n-types :uint32))

(defcstruct trace-db
  (traces (:pointer (:struct c-trace)))
  (n-traces :uint64)
  (n-traces-allocated :uint64))

(defclass binary-trace-db (trace-db)
  ((db-pointer :reader db-pointer)
   (trace-metadata :initform nil :accessor trace-metadata :type 'list)))

(defmethod initialize-instance :after ((instance binary-trace-db) &key)
  (load-libtrace-db)
  (let ((db-pointer (create-db)))
    (setf (slot-value instance 'db-pointer) db-pointer)
    (finalize instance (lambda () (free-db db-pointer)))))

(defvar *binary-trace-db-obj-code* (register-code 255 'binary-trace-db)
  "Object code for serialization of binary-trace-db software objects.")

(defstore-cl-store (obj binary-trace-db stream)
  (let ((trace-list (iter (for i from 0 to (1- (n-traces obj)))
                          (collect (get-trace obj i)))))
    (output-type-code *binary-trace-db-obj-code* stream)
    (cl-store::store-object trace-list stream)))

(defrestore-cl-store (binary-trace-db stream)
  (let ((trace-db (make-instance 'binary-trace-db))
        (trace-list (cl-store::restore-object stream)))
    (iter (for (trace . metadata) in trace-list)
          (for i upfrom 0)
          (set-trace trace-db i (cdr trace) metadata))
    trace-db))

(defun get-trace-struct (db index)
  "Return the C trace struct."
  (let ((db-struct (mem-aref (db-pointer db) '(:struct trace-db))))
    (assert (< index (getf db-struct 'n-traces)))
    (mem-aref (getf db-struct 'traces) '(:struct c-trace) index)))

(defmethod n-traces ((db binary-trace-db))
  "Return the number of traces stored in DB."
  (getf (mem-aref (db-pointer db) '(:struct trace-db))
        'n-traces))

(defmethod trace-size ((db binary-trace-db) index)
  "Return the number of points in the trace at INDEX in DB."
  (getf (get-trace-struct db index) 'n-points))

(defmethod add-trace ((db binary-trace-db) filename timeout
                      metadata &key max)
  (let ((state-pointer (start-reading (namestring filename) timeout)))
    (assert (not (null-pointer-p state-pointer)))
    (c-add-trace (db-pointer db) state-pointer
                 (or max 0))
    (push metadata (trace-metadata db))))

(defmethod trace-types ((db binary-trace-db) index)
  "Return the type names from the header of the trace at INDEX in DB."
  (let* ((trace (get-trace-struct db index))
         (names (iter (with ptr = (getf trace 'names))
                      (for i below (getf trace 'n-names))
                      (for str = (mem-aref ptr :string i))
                      (collect str result-type 'vector))))
    (iter (with ptr = (getf trace 'types))
          (for i below (getf trace 'n-types))
          (collect
              (aref names
                    (type-name-index (mem-aref ptr
                                               '(:struct type-description)
                                               i)))))))

(defun convert-results (db index n-results results-ptr
                        &key filter)
  (let* ((trace (get-trace-struct db index))
         (names (iter (with ptr = (getf trace 'names))
                      (for i below (getf trace 'n-names))
                      (for str = (mem-aref ptr :string i))
                      (while str)
                      (collect str result-type 'vector)))
         (types (iter (with ptr = (getf trace 'types))
                      (for i below (getf trace 'n-types))
                      (for str =
                           (mem-aref ptr '(:struct type-description) i))
                      (while str)
                      (collect str result-type 'vector))))
      (iter (for i below n-results)
            (let ((point (convert-trace-point names
                                              types
                                              results-ptr
                                              i)))
              (when (or (not filter)
                        (apply filter
                               (cdr (assoc :c point))
                               (cdr (assoc :scopes point))))
                (collect point))))))

(defmethod get-trace ((db binary-trace-db) index &key file-id)
  (let* ((struct (get-trace-struct db index))
         (points (convert-results db index
                                  (getf struct 'n-points)
                                  (getf struct 'points))))
    (cons (cons :trace (if file-id
                           (remove-if-not [{eq file-id} #'cdr {assoc :f}]
                                          points)
                           points))
          (elt (trace-metadata db) index))))


;;; Database queries
(defcstruct free-variable
  (n-allowed-types :uint32)
  (allowed-types (:pointer :uint32)))

(defcenum predicate-kind
  :var-reference
  :var-size
  :var-value
  :distinct-vars
  :signed-integer
  :unsigned-integer
  :and
  :or
  :greater-than
  :less-than
  :equal
  :add
  :subtract
  :multiply
  :divide)

(defcstruct predicate
  (kind predicate-kind)
  (data :uint64)
  (children (:pointer (:struct predicate))))

(defcfun ("query_trace" c-query-trace) :void
  (db (:pointer (:struct trace-db)))
  (index :uint64)
  (n-variables :uint32)
  (variables (:pointer (:struct free-variable)))
  (predicate (:pointer (:struct predicate)))
  (seed :uint32)
  (statement-mask :uint64)
  (statement :uint64)
  (results-out :pointer)
  (n-results :pointer))

(defcfun free-query-result :void
  (results-out :pointer)
  (n-results :uint64))

(defcfun free-predicate :void
  (predicate :pointer))

(defun create-foreign-array (type contents)
  "Allocate a foreign array and fill it with the contents of a Lisp array."
  (let* ((length (length contents))
         (array (foreign-alloc type :count length)))
    (lisp-array-to-foreign contents array
                           `(:array ,type ,length))
    array))

(defun build-predicate (var-names predicate)
  "Build a predicate struct from an s-expression."
  (labels
      ((build-var-reference (var)
         (if-let ((index (position var var-names)))
           (list 'kind :var-reference
                 'data index
                 'children (null-pointer))
           (error "undefined variable in predicate: ~a" var)))
       (build-number (number)
         (list 'kind (if (negative-integer-p number)
                         :signed-integer
                         :unsigned-integer)
               'data number
               'children (null-pointer)))
       (build-operator (expr)
         (destructuring-bind (op . args) expr
           (list
            'kind (ecase op
                    (distinct :distinct-vars)
                    (or :or)
                    (and :and)
                    (v/size :var-size)
                    (v/value :var-value)
                    (> :greater-than)
                    (< :less-than)
                    (= :equal)
                    (+ :add)
                    (- :subtract)
                    (* :multiply)
                    (/ :divide))
            'data (length args)
            'children (create-foreign-array '(:struct predicate)
                                            (map 'vector #'helper args)))))
       (helper (tree)
         (cond
           ((listp tree) (build-operator tree))
           ((numberp tree) (build-number tree))
           (t (build-var-reference tree)))))

      (if predicate
       (let ((result (foreign-alloc '(:struct predicate))))
         (setf (mem-aref result '(:struct predicate))
               (helper predicate))
         result)
       (null-pointer))))

(defmethod query-trace ((db binary-trace-db) index var-names var-types
                        &key pick file-id predicate filter)
  (assert (< index (n-traces db)))
  (when predicate (assert var-names))
  (let* ((n-vars (length var-types))
         (trace-types (trace-types db index))
         ;; Convert type names to indices
         (type-indices
          (mapcar {map 'vector {position _ trace-types :test #'string=}}
                  var-types))
         ;; Create an array of free_variable structs
         (free-vars
          (let ((var-array (foreign-alloc '(:struct free-variable)
                                          :count n-vars)))
            (iter (for types in type-indices)
                  (for i upfrom 0)
                  (for type-array = (create-foreign-array :uint32 types))
                  (setf (mem-aref var-array '(:struct free-variable) i)
                        `(n-allowed-types ,(length types)
                          allowed-types ,type-array)))
            var-array))
         (predicate-ptr (build-predicate var-names predicate)))
    (with-foreign-object (results-ptr '(:pointer (:struct trace-point)))
      (setf (mem-aref results-ptr '(:pointer (:struct trace-point)))
            (null-pointer))
      (with-foreign-object (n-results-ptr ':uint64)
        (setf (mem-aref n-results-ptr ':uint64) 0)
        (unwind-protect
             (progn
               (c-query-trace (db-pointer db) index
                              n-vars free-vars predicate-ptr
                              (if pick (random (expt 2 32)) 0)
                              (if file-id
                                  (ash (1- (ash 1 +trace-id-file-bits+))
                                       +trace-id-statement-bits+)
                                  0)
                              (if file-id
                                  (ash file-id +trace-id-statement-bits+)
                                  0)
                              results-ptr n-results-ptr)
               (convert-results db index
                                (mem-ref n-results-ptr :uint64)
                                (mem-ref results-ptr :pointer)
                                :filter filter))

         (progn
           ;; Free memory
           (free-query-result (mem-ref results-ptr :pointer)
                              (mem-ref n-results-ptr :uint64))
           (free-predicate predicate-ptr)
           (iter (for i below n-vars)
                 (foreign-free (getf (mem-aref free-vars
                                               '(:struct free-variable) i)
                                     'allowed-types)))
           (foreign-free free-vars)))))))

(defcfun ("set_trace" c-set-trace) :void
  (db :pointer)
  (index :uint32)
  (trace :pointer))


(defmethod set-trace ((db binary-trace-db) index trace &optional metadata)
  ;; This is intended primarily for testing. It does not handle blobs and
  ;; may not be particularly efficient
  (assert (<= index (n-traces db)))

  (let* ((type-hash (make-hash-table :test #'equal))
         (var-names (mappend (lambda (point)
                               (mapcar (lambda (var-info)
                                         (let ((type (elt var-info 1)))
                                           (setf (gethash type type-hash)
                                                 (or (< (elt var-info 2) 0)
                                                     (gethash type type-hash))))
                                         (elt var-info 0))
                                       (cdr (assoc :scopes point))))
                             trace))
         (type-names (hash-table-keys type-hash))
         (all-names (append type-names
                            (remove-duplicates var-names :test #'string=)))
         (name-hash (iter (with ht = (make-hash-table :test #'equal))
                          (for name in all-names)
                          (for i upfrom 0)
                          (setf (gethash name ht) i)
                          (finally (return ht))))
         (types
          (iter (for name in type-names)
                (collect
                    `(name-index ,(gethash name name-hash)
                      format ,(if (gethash name type-hash) :signed :unsigned)
                      size 8)
                  result-type 'vector)))
         (points
          (mapcar
           (lambda (point)
             (let ((vars (mapcar
                          (lambda (var-info)
                            (let ((has-size (and (= 4 (length var-info))
                                                 (elt var-info 3))))
                              `(value ,(elt var-info 2)
                                      name-index ,(gethash (elt var-info 0)
                                                           name-hash)
                                      type-index ,(gethash (elt var-info 1)
                                                           name-hash)
                                      has-buffer-size ,(if has-size 1 0)
                                      buffer-size ,(if has-size
                                                       (elt var-info 3) 0))))
                          (cdr (assoc :scopes point)))))

               `(statement ,(if (cdr (assoc :f point))
                                (logior (ash 1 63)                      ;flag bit
                                        (ash (cdr (assoc :f point))
                                             +trace-id-statement-bits+) ;file ID
                                        (cdr (assoc :c point)))         ;AST ID
                                (cdr (assoc :c point)))
                           n-sizes 0 sizes ,(null-pointer)
                           n-aux 0 aux ,(null-pointer)
                           n-vars ,(length vars)
                           vars ,(create-foreign-array '(:struct var-info)
                                                       (coerce vars 'vector)))))
           trace))
         (trace-struct (foreign-alloc '(:struct c-trace))))
    (setf (mem-ref trace-struct '(:struct c-trace))
          `(points ,(create-foreign-array '(:struct trace-point)
                                          (coerce points 'vector))
            n-points ,(length points)
            n-points-allocated ,(length points)
            names ,(create-foreign-array :string (coerce all-names 'vector))
            n-names ,(length all-names)
            types ,(create-foreign-array '(:struct type-description) types)
            n-types ,(length types)))
    (c-set-trace (db-pointer db) index trace-struct)
    (if (< index (length (trace-metadata db)))
        (setf (nth index (trace-metadata db)) metadata)
        (appendf (trace-metadata db) (list metadata)))))

(defclass single-file-binary-trace-db (binary-trace-db)
  ((file-id :reader file-id :type number)
   (parent-db :initarg :parent-db :type binary-trace-db :accessor parent-db))
  (:documentation
   "Wrapper around binary-trace-db which restricts queries to one file of a
project."))

(defvar *single-file-binary-trace-db-obj-code*
  (register-code 256 'single-file-binary-trace-db)
  "Object code for serialization of single-file-trace-db software objects.")

(defstore-cl-store (obj single-file-binary-trace-db stream)
  ;; Do not serialize single file trace databases
  (output-type-code *single-file-binary-trace-db-obj-code* stream)
  (cl-store::store-object nil stream))

(defrestore-cl-store (single-file-binary-trace-db stream)
  (cl-store::restore-object stream))

(defmethod restrict-to-file ((db binary-trace-db) file-id)
  "Return a wrapper around DB which restricts results by FILE-ID."
  ;; New instance is created without a db-pointer, so it will not
  ;; create a finalizer. The original DB is still the owner of the
  ;; database and will free it.
  (let ((result (make-instance 'single-file-binary-trace-db
                               :parent-db db)))
    (setf (slot-value result 'file-id) file-id)
    (setf (slot-value result 'db-pointer) (db-pointer db))
    result))

(defmethod query-trace :around ((db single-file-binary-trace-db) index
                                var-names var-types
                                &key pick file-id predicate filter)
  (call-next-method db index var-names var-types
                    :pick pick :predicate predicate :filter filter
                    :file-id (or file-id (file-id db))))

(defmethod get-trace :around ((db single-file-binary-trace-db) index
                              &key file-id)
  (call-next-method db index :file-id (or file-id (file-id db))))

(defmethod trace-metadata ((db single-file-binary-trace-db))
  (or (slot-value db 'trace-metadata) (trace-metadata (parent-db db))))