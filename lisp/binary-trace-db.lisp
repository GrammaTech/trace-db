;; binary-trace-db.lisp - Trace database which uses a proprietary trace
;; format and C backend.

(in-package :trace-db)
(in-readtable :curry-compose-reader-macros)

(define-constant +lib-dir+
    (make-pathname :directory (pathname-directory
                               #.(or *compile-file-truename*
                                     *load-truename*
                                     *default-pathname-defaults*)))
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
  :blob
  :invalid-format)

(defcstruct (var-info :class c-trace-var-info-struct)
  ;; CFFI's handling of the union here seems to be broken.
  ;; Use an integer here to get the correct layout, and figure out the
  ;; real type when we dereference.
  (value :uint64)
  (format type-format)
  (var-name-index :uint32)
  (type-name-index :uint32)
  (size :uint32)
  (buffer-size :uint64)
  (has-buffer-size :uint8))

(defcstruct (trace-point :class c-trace-point-struct)
  (statement :uint64)
  (vars (:pointer (:struct var-info)))
  (n-vars :uint32)
  (aux (:pointer :uint64))
  (n-aux :uint32))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod expand-from-foreign (info (type c-trace-var-info-struct))
    `(with-foreign-slots ((format var-name-index type-name-index size
                           has-buffer-size buffer-size)
                          ,info
                          (:struct var-info))
       (make-array 4 :initial-contents
         (list var-name-index
               type-name-index
               (let* ((value (mem-ref ,info
                                      (ecase format
                                        (:unsigned :uint64)
                                        (:signed :int64)
                                        (:float
                                         (if (= 4 size) :float :double))
                                        (:pointer :uint64)
                                        (:blob :pointer))
                                      0)))
                 (if (eq :blob format)
                     ;; Blob: convert to string, or collect into byte array.
                     (restart-case
                         (foreign-string-to-lisp value :count size)
                       (store-as-byte-array ()
                         :report "Store as a byte array"
                         (iter (for i below size)
                               (collecting (mem-ref value :char i)
                                           result-type 'vector))))
                     ;; Primitive type
                     value))
               (unless (zerop has-buffer-size)
                 buffer-size)))))
  (defmethod expand-from-foreign (point (type c-trace-point-struct))
    `(with-foreign-slots ((statement vars n-vars aux n-aux)
                          ,point
                          (:struct trace-point))
       (list ;; SEL clang-project instrumentation packs file and AST
             ;; indices into the trace ID, with the top bits used as a flag
             ;; to indicate the presence of the file ID.
             ;; Since SEL is likely to be the only Lisp client, it's
             ;; convenient to handle this here.
             (cons :c
                   (if (> (logand statement (ash 1 63)) 0)
                       (logand (1- (ash 1 +trace-id-statement-bits+))
                               statement)
                       statement))
             (cons :f
                   (when (> (logand statement (ash 1 63)) 0)
                     (logand (1- (ash 1 +trace-id-file-bits+))
                             (ash statement
                                  (* -1 +trace-id-statement-bits+)))))
             (cons :scopes
                   (when (> n-vars 0)
                     (iter (for i below n-vars)
                           (collect (mem-aref vars
                                              '(:struct var-info)
                                              i)))))
             (cons :aux
                   (when (> n-aux 0)
                     (foreign-array-to-lisp aux `(:array :uint64 ,n-aux))))))))


(defconstant +trace-id-file-bits+ 23
  "Number of bits in trace ID used to identify the file.")
(defconstant +trace-id-statement-bits+ 40
  "Number of bits in trace ID used to identify the statement within a file.")

(defcfun ("read_trace" c-read-trace) :pointer
  (filename :string)
  (timeout :uint32)
  (max-points :uint64))

(defcfun ("trace_size" c-trace-size) :uint64
  (trace :pointer))

(defcfun ("get_points" c-get-points) :pointer
  (trace :pointer))

(defcfun ("n_types" c-n-types) :uint32
  (trace :pointer))

(defcfun ("get_types" c-get-types) (:pointer :string)
  (trace :pointer))

(defcfun ("n_names" c-n-names) :uint32
  (trace :pointer))

(defcfun ("get_names" c-get-names) (:pointer :string)
  (trace :pointer))

(defcfun ("free_trace" c-free-trace) :void
  (trace-ptr :pointer))

(defcfun ("free_trace_points" c-free-trace-points) :void
  (points :pointer)
  (n_points :uint64))

(defun convert-trace-points (trace-ptr trace-points-ptr n-points
                             &key (filter (constantly t)))
  "Convert the given list of foreign trace points (TRACE-POINTS-PTR) to
common lisp.  A FILTER may be given to exclude some trace points from
the results."
  (labels ((get-names (trace-ptr)
             (let ((n-names (c-n-names trace-ptr))
                   (names-ptr (c-get-names trace-ptr)))
               (prog1
                 (let ((names (iter (for i below n-names)
                                    (collect (mem-aref names-ptr :string i)))))
                   (make-array (length names) :initial-contents names))
                 (foreign-free names-ptr))))
           (convert-trace-point (point names)
             (iter (for var in (cdr (assoc :scopes point)))
                   (setf (elt var 0) (aref names (elt var 0))
                         (elt var 1) (aref names (elt var 1))))
             point))
    (prog1
      (iter (with names = (get-names trace-ptr))
            (for i below n-points)
            (when-let* ((point (convert-trace-point
                                 (mem-aref trace-points-ptr
                                           '(:struct trace-point)
                                           i)
                                 names))
                        (_ (apply filter
                                  (cdr (assoc :f point))
                                  (cdr (assoc :c point))
                                  (cdr (assoc :scopes point)))))
              (collect point)))
      (c-free-trace-points trace-points-ptr n-points))))

(defun read-binary-trace (file &key (timeout 0) (max 0))
  "Read a trace and convert to a list."
  (assert (and (integerp timeout) (not (negative-integer-p timeout)))
          (timeout)
          "TIMEOUT must be a non-negative integer value.")
  (assert (and (integerp max) (not (negative-integer-p max)))
          (max)
          "MAX must be a non-negative integer value.")

  (load-libtrace-db)
  (let ((trace-ptr (c-read-trace file timeout max)))
    (unless (null-pointer-p trace-ptr)
      (convert-trace-points trace-ptr
                            (c-get-points trace-ptr)
                            (c-trace-size trace-ptr)))))


;;; Trace DB interface
(defcfun ("create_db" c-create-db) :pointer)
(defcfun ("add_trace" c-add-trace) :int
  (db :pointer)
  (filename :string)
  (timeout :int)
  (max :uint64))
(defcfun ("add_trace_points" c-add-trace-points) :int
  (db :pointer)
  (points :pointer)
  (n-points :uint64)
  (names (:pointer :string))
  (n-names :uint32))
(defcfun ("get_trace" c-get-trace) :pointer
  (db :pointer)
  (index :uint32))
(defcfun ("n_traces" c-n-traces) :uint32
  (db :pointer))
(defcfun ("serialize_trace_db" c-serialize-trace-db) :string
  (db :pointer))
(defcfun ("deserialize_trace_db" c-deserialize-trace-db) :pointer
  (text :string))
(defcfun ("free_db" c-free-db) :void
  (db :pointer))

(defclass binary-trace-db (trace-db)
  ((db-pointer     :initarg :db-pointer
                   :accessor db-pointer
                   :initform nil)
   (trace-metadata :initarg :trace-metadata
                   :accessor trace-metadata
                   :type 'list
                   :initform nil)))

(defmethod initialize-instance :after ((instance binary-trace-db) &key)
  (load-libtrace-db)
  (when (null (db-pointer instance))
    (setf (db-pointer instance) (c-create-db))
    (finalize instance (lambda () (c-free-db (db-pointer instance))))))

(defvar *binary-trace-db-obj-code* (register-code 255 'binary-trace-db)
  "Object code for serialization of binary-trace-db software objects.")

(defstore-cl-store (obj binary-trace-db stream)
  (output-type-code *binary-trace-db-obj-code* stream)
  (cl-store::store-object `((:db . ,(when (db-pointer obj)
                                      (c-serialize-trace-db (db-pointer obj))))
                            (:trace-metadata . ,(trace-metadata obj)))
                          stream))

(defrestore-cl-store (binary-trace-db stream)
  (load-libtrace-db)
  (let* ((restore (cl-store::restore-object stream))
         (db-pointer (when (not (emptyp (cdr (assoc :db restore))))
                       (c-deserialize-trace-db (cdr (assoc :db restore)))))
         (trace-metadata (cdr (assoc :trace-metadata restore))))
    (make-instance 'binary-trace-db :db-pointer db-pointer
                                    :trace-metadata trace-metadata)))

(defmethod n-traces ((db binary-trace-db))
  "Return the number of traces stored in DB."
  (c-n-traces (db-pointer db)))

(defmethod trace-size ((db binary-trace-db) (index integer))
  "Return the number of points in the trace at INDEX in DB."
  (assert (and (<= 0 index) (< index (n-traces db))) (index)
          "INDEX must be in range [0, n-traces).")
  (c-trace-size (c-get-trace (db-pointer db) index)))

(defmethod add-trace ((db binary-trace-db)
                      (filename string)
                      (timeout integer)
                      (metadata list)
                      &key max)
  (assert (<= 0 timeout) (timeout)
          "TIMEOUT must be a non-negative integer value.")
  (assert (or (null max) (<= 0 max)) (max)
          "MAX must be a non-negative integer value.")

  (let ((success (c-add-trace (db-pointer db) filename timeout (or max 0))))
    (unless (zerop success)
      (push metadata (trace-metadata db))
      t)))

(defmethod add-trace-points ((db binary-trace-db) (trace list)
                             &optional metadata)
  "Directly add a list of trace points to the database.

IMPORTANT: This method is for legacy testing purposes only.
This method is not complete or efficient.
DO NOT USE IN PRODUCTION OR NEW DEVELOPMENT."
  (let ((names (remove-duplicates
                 (mappend (lambda (point)
                            (mappend (lambda (var)
                                       (list (elt var 0) (elt var 1)))
                                     (cdr (assoc :scopes point))))
                          trace)
                 :test #'string=)))
    (c-add-trace-points
      (db-pointer db)
      (create-foreign-array
        '(:struct trace-point)
        (coerce
          (mapcar
            (lambda (point)
              (let ((vars (mapcar
                            (lambda (var-info)
                              (let ((has-size (and (= 4 (length var-info))
                                                   (elt var-info 3))))
                                `(value ,(elt var-info 2)
                                  format ,(if (< (elt var-info 2) 0)
                                              :signed :unsigned)
                                  var-name-index ,(position (elt var-info 0)
                                                            names
                                                            :test #'string=)
                                  type-name-index ,(position (elt var-info 1)
                                                             names
                                                             :test #'string=)
                                  size 8
                                  has-buffer-size ,(if has-size 1 0)
                                  buffer-size ,(if has-size
                                                  (elt var-info 3)
                                                  0))))
                            (cdr (assoc :scopes point)))))
                `(statement
                  ,(if (cdr (assoc :f point))
                       (logior (ash 1 63)                      ;flag bit
                               (ash (cdr (assoc :f point))
                                    +trace-id-statement-bits+) ;file ID
                               (cdr (assoc :c point)))         ;AST ID
                       (cdr (assoc :c point)))
                  vars ,(create-foreign-array
                           '(:struct var-info)
                           (coerce vars 'vector))
                  n-vars ,(length vars)
                  aux ,(null-pointer)
                  n-aux 0)))
            trace)
          'vector))
      (length trace)
      (create-foreign-array :string (coerce names 'vector))
      (length names))
    (push metadata (trace-metadata db))))

(defmethod trace-types ((db binary-trace-db) (index integer))
  "Return the type names from the header of the trace at INDEX in DB."
  (assert (and (<= 0 index) (< index (n-traces db))) (index)
          "INDEX must be in range [0, n-traces).")

  (let* ((trace-ptr (c-get-trace (db-pointer db) index))
         (n-types (c-n-types trace-ptr))
         (types-ptr (c-get-types trace-ptr)))
    (prog1
      (iter (for i below n-types)
            (collect (mem-aref types-ptr :string i)))
      (foreign-free types-ptr))))

(defmethod get-trace ((db binary-trace-db) (index integer) &key file-id)
  "Return the trace points in the trace at INDEX in DB."
  (assert (and (<= 0 index) (< index (n-traces db))) (index)
          "INDEX must be in range [0, n-traces).")

  (let* ((trace-ptr (c-get-trace (db-pointer db) index))
         (points (convert-trace-points trace-ptr
                                       (c-get-points trace-ptr)
                                       (c-trace-size trace-ptr))))
    (cons (cons :trace (if file-id
                           (remove-if-not [{eq file-id} #'cdr {assoc :f}]
                                          points)
                           points))
          (elt (trace-metadata db) index))))


;;; Database queries
(defcenum predicate-kind
  :null-predicate
  :var-reference
  :var-size
  :var-value
  :distinct-vars
  :null-value
  :signed-value
  :unsigned-value
  :float-value
  :and
  :or
  :not
  :greater-than
  :greater-than-or-equal
  :less-than
  :less-than-or-equal
  :equal
  :add
  :subtract
  :multiply
  :divide)

(defcstruct c-predicate
  (kind predicate-kind)
  (n-children :uint64)
  (var-index :uint64)
  ;; CFFI does not handle union types properly so
  ;; we have a separate variable for each type
  ;; and use the appropriate value based on the
  ;; predicate kind.
  (unsigned-value :uint64)
  (signed-value :int64)
  (float-value :double)
  (children (:pointer (:struct c-predicate))))

(defcstruct c-free-variable
  (n-allowed-types :uint32)
  (allowed-types (:pointer :uint32)))

(defcfun ("query_trace" c-query-trace) :void
  (db :pointer)
  (index :uint64)
  (variables (:pointer (:struct c-free-variable)))
  (n-variables :uint32)
  (predicate (:pointer (:struct c-predicate)))
  (soft-predicates :pointer)
  (n-soft-predicates :uint32)
  (seed :uint32)
  (keep-duplicate-bindings :uint8)
  (statement-mask :uint64)
  (statement :uint64)
  (results-out :pointer)
  (n-results :pointer))

(defcfun ("free_c_predicate" c-free-c-predicate) :void
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
                 'n-children 0
                 'var-index index
                 'unsigned-value 0
                 'signed-value 0
                 'float-value 0.0D0
                 'children (null-pointer))
           (error "undefined variable ~s in var-names ~s:"
                  var var-names)))
       (build-number (number)
         (let ((kind (cond ((or (positive-integer-p number)
                                (and (integerp number) (zerop number)))
                            :unsigned-value)
                           ((negative-integer-p number)
                            :signed-value)
                           (t :float-value))))
           (list 'kind kind
                 'n-children 0
                 'var-index 0
                 'unsigned-value (if (eq :unsigned-value kind) number 0)
                 'signed-value   (if (eq :signed-value kind) number 0)
                 'float-value    (if (eq :float-value kind)
                                     (coerce number 'double-float) 0.0D0)
                 'children (null-pointer))))
       (build-operator (expr)
         (destructuring-bind (op . args) expr
           (list
            'kind (ecase op
                    (distinct :distinct-vars)
                    (and :and)
                    (or :or)
                    (not :not)
                    (v/size :var-size)
                    (v/value :var-value)
                    (> :greater-than)
                    (>= :greater-than-or-equal)
                    (< :less-than)
                    (<= :less-than-or-equal)
                    (equal :equal)
                    (= :equal)
                    (+ :add)
                    (- :subtract)
                    (* :multiply)
                    (/ :divide))
            'n-children (length args)
            'var-index 0
            'unsigned-value 0
            'signed-value 0
            'float-value 0.0D0
            'children (create-foreign-array '(:struct c-predicate)
                                            (map 'vector #'helper args)))))
       (helper (tree)
         (cond
           ((listp tree) (build-operator tree))
           ((numberp tree) (build-number tree))
           (t (build-var-reference tree)))))

    (if predicate
        (let ((result (foreign-alloc '(:struct c-predicate))))
          (setf (mem-aref result '(:struct c-predicate))
                (helper predicate))
          result)
        (null-pointer))))

(defmethod query-trace ((db binary-trace-db) index var-names var-types
                        &key pick file-id predicate soft-predicates filter)
  (assert (and (<= 0 index) (< index (n-traces db))) (index)
          "INDEX must be in range [0, n-traces).")
  (when (or predicate soft-predicates)
    (assert var-names))

  (let* ((trace-ptr (c-get-trace (db-pointer db) index))
         (n-vars (length var-types))
         (trace-types (trace-types db index))
         ;; Convert type names to indices
         (type-indices
          (mapcar {map 'vector {position _ trace-types :test #'string=}}
                  var-types))
         ;; Create an array of c_free_variable structs
         (free-vars
          (let ((var-array (foreign-alloc '(:struct c-free-variable)
                                          :count n-vars)))
            (iter (for types in type-indices)
                  (for i upfrom 0)
                  (for type-array = (create-foreign-array :uint32 types))
                  (setf (mem-aref var-array '(:struct c-free-variable) i)
                        `(n-allowed-types ,(length types)
                          allowed-types ,type-array)))
            var-array))
         ;; Create c_predicate structs
         (predicate-ptr (build-predicate var-names predicate))
         (soft-predicate-ptrs (map 'vector {build-predicate var-names}
                                   soft-predicates))
         (soft-predicate-array (create-foreign-array :pointer
                                                     soft-predicate-ptrs)))
    (with-foreign-object (results-ptr '(:pointer (:struct trace-point)))
      (setf (mem-aref results-ptr '(:pointer (:struct trace-point)))
            (null-pointer))
      (with-foreign-object (n-results-ptr ':uint64)
        (setf (mem-aref n-results-ptr ':uint64) 0)
        (unwind-protect
             (prog2
               (c-query-trace (db-pointer db)
                              index
                              free-vars
                              n-vars
                              predicate-ptr
                              soft-predicate-array
                              (length soft-predicate-ptrs)
                              (if pick (random (expt 2 32)) 0)
                              (if filter 1 0)
                              (if file-id
                                  (ash (1- (ash 1 +trace-id-file-bits+))
                                       +trace-id-statement-bits+)
                                  0)
                              (if file-id
                                  (ash file-id +trace-id-statement-bits+)
                                  0)
                              results-ptr
                              n-results-ptr)
               ;; Convert all results and filter them
               (cons (cons :results
                           (if filter
                               ;; Deduplicate results with the same statement
                               ;; and variable bindings on the Common LISP
                               ;; side when a filter is given so that
                               ;; each point may go thru the filter.
                               (remove-duplicates
                                 (convert-trace-points trace-ptr
                                                       (mem-ref results-ptr
                                                                :pointer)
                                                       (mem-ref n-results-ptr
                                                                :uint64)
                                                       :filter filter)
                                 :key #'get-statement-and-bindings
                                 :test #'equalp)
                               ;; No filter, results are deduplicated
                               ;; on the C-side of the query interface.
                               (convert-trace-points trace-ptr
                                                     (mem-ref results-ptr
                                                              :pointer)
                                                     (mem-ref n-results-ptr
                                                              :uint64))))
                     (nth index (trace-metadata db))))
          (progn
            (c-free-c-predicate predicate-ptr)
            (iter (for i below (length soft-predicate-ptrs))
                  (c-free-c-predicate (aref soft-predicate-ptrs i)))
            (foreign-free soft-predicate-array)
            (iter (for i below n-vars)
                  (foreign-free (getf (mem-aref free-vars
                                                '(:struct c-free-variable) i)
                                      'allowed-types)))
            (foreign-free free-vars)))))))


;;; Single-file binary trace database
(defclass single-file-binary-trace-db (binary-trace-db)
  ((file-id :initarg :file-id
            :reader file-id
            :type number)
   (parent-db :initarg :parent-db
              :reader parent-db
              :type binary-trace-db))
  (:documentation
   "Wrapper around binary-trace-db which restricts queries to one file of a
project."))

(defvar *single-file-binary-trace-db-obj-code*
  (register-code 256 'single-file-binary-trace-db)
  "Object code for serialization of single-file-binary-trace-db
software objects.")

(defstore-cl-store (obj single-file-binary-trace-db stream)
  ;; Do not serialize single file trace databases
  (output-type-code *single-file-binary-trace-db-obj-code* stream)
  (cl-store::store-object nil stream))

(defrestore-cl-store (single-file-binary-trace-db stream)
  (cl-store::restore-object stream))

(defmethod restrict-to-file ((db binary-trace-db) file-id)
  "Return a wrapper around DB which restricts results by FILE-ID."
  (make-instance 'single-file-binary-trace-db
    :parent-db db
    :db-pointer (db-pointer db)
    :file-id file-id))

(defmethod query-trace :around ((db single-file-binary-trace-db) index
                                var-names var-types
                                &key pick file-id predicate
                                  soft-predicates filter)
  (call-next-method db index var-names var-types
                    :pick pick
                    :predicate predicate
                    :soft-predicates soft-predicates
                    :filter filter
                    :file-id (or file-id (file-id db))))

(defmethod get-trace :around ((db single-file-binary-trace-db) index
                              &key file-id)
  (call-next-method db index :file-id (or file-id (file-id db))))

(defmethod trace-metadata ((db single-file-binary-trace-db))
  (or (slot-value db 'trace-metadata) (trace-metadata (parent-db db))))
