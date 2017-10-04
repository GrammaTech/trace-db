(in-package :libtrace)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (format t "Loading libtrace.so from lib-dir ~a~%" +lib-dir+)
  (pushnew +lib-dir+ *foreign-library-directories*
           :test #'equal)

  (define-foreign-library libtrace
    (t (:default "libtrace")))

  (use-foreign-library libtrace)

  (format t "Done loading libtrace.so"))

;; (defcenum type-format
;;   :unsigned
;;   :signed
;;   :float
;;   :pointer
;;   :blob)

(defcstruct (buffer-size :class c-buffer-size)
  (address :uint64)
  (size :uint64))

;; (defcstruct (type-description :class c-type-description)
;;   (name-index :uint16)
;;   (format type-format)
;;   (size :uint8))

;; (defstruct (type-description (:conc-name type-))
;;   (name-index nil :type fixnum)
;;   (format nil :type symbol)
;;   (size nil :type (unsigned-byte 8)))

(defcstruct (var-info :class c-var-info)
  ;; CFFI's handling of the union here seems to be broken.
  ;; Use an integer here to get the correct layout, and figure out the
  ;; real type when we dereference.
  (value :int64)
  (name-index :uint16)
  (type-index :uint16)
  (size :uint16))

;; (defcstruct trace-read-state
;;   (file :pointer)
;;   (names (:pointer :string))
;;   (n-names :uint16)
;;   (types (:pointer (:struct type-description)))
;;   (n-types :uint16))

(defcstruct (trace-point :class c-trace-point)
  (statement :uint32)
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
    `(with-foreign-slots ((name-index type-index size) ,info
                          (:struct var-info))
       `(,name-index ,type-index ,size)))
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

;; (defcfun start-reading (:pointer (:struct trace-read-state))
;;   (filename :string)
;;   (timeout :int))

;; (defcfun end-reading :void
;;   (state :pointer))

;; (defcfun read-trace-point :int
;;   (state :pointer) (results :pointer))

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
               (destructuring-bind (name-index type-index size)
                   (mem-aref vars '(:struct var-info) i)
                 (let ((type (aref types type-index)))
                   (collect `#(,(aref names name-index)
                               ,(aref names (type-name-index type))
                               ,(get-value vars type size i)))))))
       (read-sizes (sizes count)
         (iter (for i below count)
               (collect (mem-aref sizes '(:struct buffer-size) i)))))
    (destructuring-bind (statement sizes n-sizes vars n-vars aux n-aux)
        (mem-aref point '(:struct trace-point) index)
      (when (> n-sizes 0)
        (push (cons :sizes (read-sizes sizes n-sizes))
              result))
      (when (> n-vars 0)
        (push (cons :scopes (read-vars vars n-vars))
              result))
      (when (> n-aux 0)
        (push (cons :aux
                    (foreign-array-to-lisp aux `(:array :uint64 ,n-aux)))
              result))
      (when (> statement 0)
        ;; SEL clang-project instrumentation uses 16 bits for
        ;; statement ID, 15 for file ID, and the top bit as a flag
        ;; indicating file ID is present.
        ;; Since SEL is likely to be the only Lisp client, it's
        ;; convenient to handle this here.
        (if (> (logand statement (ash 1 31)) 0)
            (progn (push (cons :c (logand #xFFFF statement)) result)
                   (push (cons :f (logand #x7FFF (ash statement -16))) result))
         (push (cons :c statement) result)))))
  result)

(defun read-trace (file timeout &key (predicate #'identity) max
                   &aux (collected 0))
  "Read a trace and convert to a list."
  (let ((state-ptr (start-reading file timeout)))
    (unless (null-pointer-p state-ptr)
      (let* ((state (mem-aref state-ptr '(:struct trace-read-state)))
             (names (iter (with ptr = (getf state 'names))
                          (for i below (getf state 'n-names))
                          (for str = (mem-aref ptr :string i))
                          (while str)
                          (collect str result-type 'vector)))
             (types (iter (with ptr = (getf state 'types))
                          (for i below (getf state 'n-types))
                          (for str =
                               (mem-aref ptr '(:struct type-description) i))
                          (while str)
                          (collect str result-type 'vector))))
        (unwind-protect
             (with-foreign-object (point-struct '(:struct trace-point))
               (iter (while
                         (and (eq 0 (read-trace-point state-ptr point-struct))
                              (or (null max) (< collected max))))
                     (for trace-point =
                          (convert-trace-point names types point-struct))
                     (when (funcall predicate trace-point)
                       (incf collected)
                       (collect trace-point))))
          (end-reading state-ptr))))))
