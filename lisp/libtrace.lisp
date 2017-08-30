(in-package :libtrace)

(define-constant +lib-dir+
    (make-pathname :directory (butlast (pathname-directory
                                        #.(or *compile-file-truename*
                                              *load-truename*
                                              *default-pathname-defaults*))))
  :test #'equalp
  :documentation "Path to directory holding shared library.")

(pushnew +lib-dir+ *foreign-library-directories*
         :test #'equal)

(define-foreign-library libtrace
      (t (:default "libtrace")))
(use-foreign-library libtrace)

(defcenum entry-tag
  (:end-entry 0)
  :statement
  :scopes
  :sizes)

(defcstruct (buffer-size :class c-buffer-size)
  (address :uint64)
  (size :uint64))

(defmethod expand-from-foreign (info (type c-buffer-size))
  `(with-foreign-slots ((address size) ,info (:struct buffer-size))
     (cons address size)))

(defcstruct (var-info :class c-var-info)
  (name-index :uint8)
  (type-index :uint8)
  (value :uint64))

(defcstruct (trace-entry :class c-trace-entry)
  (kind entry-tag)
  (data :uint64))

(defcstruct trace-read-state
  (file :pointer)
  (dictionary (:pointer :string)))

(defcfun start-reading (:pointer trace-read-state)
  (filename :string))

(defcfun end-reading :void
  (state :pointer))

(defcstruct (trace-point :class c-trace-point)
  (statement :uint64)
  (sizes (:pointer (:struct buffer-size)))
  (n-sizes :uint32)
  (vars (:pointer (:struct var-info)))
  (n-vars :uint32))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod expand-from-foreign (info (type c-trace-entry))
  `(with-foreign-slots ((kind data) ,info (:struct trace-entry))
     (cons kind data)))
  (defmethod expand-from-foreign (info (type c-var-info))
    `(with-foreign-slots ((name-index type-index value) ,info (:struct var-info))
       `#(,name-index ,type-index ,value)))
  (defmethod expand-from-foreign (point (type c-trace-point))
  `(with-foreign-slots ((statement sizes n-sizes vars n-vars)
                        ,point (:struct trace-point))
     (list statement sizes n-sizes vars n-vars))))


(defcfun read-trace-point :int
  (state :pointer) (results :pointer))

(defcfun read-many-points :int
  (state :pointer) (result :pointer) (limit :uint32))

;;; Low-level interface.
;;; This is slower than reading entire trace points, probably due to
;;; CFFI overhead. Because it uses structs returned on the stack, it
;;; requires cffi-libffi.
#+nil
(defcfun read-entry (:struct trace-entry)
  (state :pointer))

#+nil
(defcfun read-var-info (:struct var-info)
  (state :pointer))

#+nil
(defcfun read-buffer-size (:struct buffer-size)
  (state :pointer))

#+nil
(defun test-read (file)
  (let* ((state (start-reading file))
         (dict (iter (with ptr =
                           (getf (mem-aref state '(:struct trace-read-state))
                                 'dictionary))
                     (for i upfrom 0)
                     (for str = (mem-aref ptr :string i))
                     (while str)
                     (collect str result-type 'array))))
    (flet ((read-vars (count)
             (iter (for i below count)
                   (let ((var (read-var-info state)))
                     (collect `#(,(aref dict (aref var 0))
                                 ,(aref dict (aref var 1))
                                 ,(aref var 2)))))))
     (unwind-protect
          (iter (for point =
                     (iter (for entry = (read-entry state))
                           (until (eq (car entry) :end-entry))
                           (ecase (car entry)
                             (:statement (collect (cons :c (cdr entry))))
                             (:scopes
                              (collect (cons :scopes (read-vars (cdr entry)))))
                             (:sizes
                              (collect
                                  (cons :sizes
                                        (iter (for i below (cdr entry))
                                              (collect (read-buffer-size state)))))))))
                (while point)
                (collect point))
       (end-reading state)))))

(defun convert-trace-point (dict point &optional (index 0) &aux result)
  (flet ((read-vars (vars count)
           (iter (for i below count)
                 (let ((var (mem-aref vars '(:struct var-info) i)))
                   (collect `#(,(aref dict (aref var 0))
                               ,(aref dict (aref var 1))
                               ,(aref var 2))))))
         (read-sizes (sizes count)
           (iter (for i below count)
                 (collect (mem-aref sizes '(:struct buffer-size) i)))))
    (destructuring-bind (statement sizes n-sizes vars n-vars)
        (mem-aref point '(:struct trace-point) index)
      (when (> n-sizes 0)
        (push (cons :sizes (read-sizes sizes n-sizes))
              result))
      (when (> n-vars 0)
        (push (cons :scopes (read-vars vars n-vars))
              result))
      (when (> statement 0)
        (push (cons :c statement) result))))
  result)

;; Reading entire trace points is significantly faster than the
;; low-level API.
(defun test-read-2 (file)
  "Read a trace, one trace point at a time."
  (let* ((state (start-reading file))
         (dict (iter (with ptr =
                           (getf (mem-aref state '(:struct trace-read-state))
                                 'dictionary))
                     (for i upfrom 0)
                     (for str = (mem-aref ptr :string i))
                     (while str)
                     (collect str result-type 'array))))
    (unwind-protect
         (with-foreign-object (point-struct '(:struct trace-point))
           (iter (while (eq 0 (read-trace-point state point-struct)))
                 (collect (convert-trace-point dict point-struct))))
      (end-reading state))))

;; Reading trace points in batches takes about the same amount of time
;; as reading them one at a time, even though it significantly reduces
;; the number of CFFI calls.
(defun test-read-3 (file)
  "Read a trace, retrieving batches of trace points."
  (let* ((limit 1024)
         (state (start-reading file))
         (dict (iter (with ptr =
                           (getf (mem-aref state '(:struct trace-read-state))
                                 'dictionary))
                     (for i upfrom 0)
                     (for str = (mem-aref ptr :string i))
                     (while str)
                     (collect str result-type 'array))))
    (unwind-protect
         (with-foreign-object (results '(:struct trace-point) limit)
           (iter (for count = (read-many-points state results limit))
                 (appending
                  (iter (for i below count)
                        (collect (convert-trace-point dict results i))))
                 (while (eq limit count))))
      (end-reading state))))
