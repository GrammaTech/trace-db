;;; sexp-trace-db.lisp --- Trace database which uses a simple
;;; s-expression trace format and LISP data structures.
;;;
;;; This is a legacy trace format, primarily for JavaScript.
;;; New clients should use the binary trace database format.

(defpackage :trace-db/sexp-trace-db
  (:use :gt/full
        :cl-store
        :trace-db/trace-db)
  (:export :read-sexp-trace
           :sexp-trace-db))
(in-package :trace-db/sexp-trace-db)
(in-readtable :curry-compose-reader-macros)

(defclass sexp-trace-db (trace-db)
  ((traces :initarg :traces
           :accessor traces
           :type list
           :initform nil)
   (trace-metadata :initarg :trace-metadata
                   :accessor trace-metadata
                   :type list
                   :initform nil)))

(defclass single-file-sexp-trace-db (sexp-trace-db)
  ((file-id :initarg :file-id
            :reader file-id
            :type number)
   (parent-db :initarg :parent-db
              :reader parent-db
              :type sexp-trace-db)))

(defvar *single-file-sexp-trace-db-obj-code*
  (register-code 254 'single-file-sexp-trace-db)
  "Object code for serialization of single-file-sexp-trace-db
software objects.")

(defstore-cl-store (obj single-file-sexp-trace-db stream)
  ;; Do not serialize single file trace databases
  (output-type-code *single-file-sexp-trace-db-obj-code* stream)
  (cl-store::store-object nil stream))

(defrestore-cl-store (single-file-sexp-trace-db stream)
  (cl-store::restore-object stream))

(defmethod initialize-instance :after ((db single-file-sexp-trace-db)
                                       &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (setf (slot-value db 'traces)
        (mapcar (lambda (trace)
                  (remove-if-not [{eq (file-id db)} #'cdr {assoc :f}] trace))
                (traces (parent-db db)))))

(defun read-sexp-trace (file &key timeout max-trace-points)
  "Read a trace from FILE-NAME with `read-trace-stream'."
  (when-let ((in (open-file-timeout file timeout)))
    (unwind-protect
        (read-trace-stream in :max-trace-points max-trace-points)
      (close in))))

(defun read-trace-stream
    (in &key (predicate #'identity) max-trace-points &aux (collected 0))
  "Read a trace from the IN.
Keyword argument PREDICATE limits collected trace points and MAX
limits number of trace points to collect."
  (iter (for trace-point =
             (restart-case (read in nil :eof)
               (ignore-rest-of-stream ()
                 :report "Ignore error and skip remainder of stream"
                 :eof)))
        (while (and (not (eq trace-point :eof))
                    (funcall predicate trace-point)
                    (or (null max-trace-points)
                        (< collected max-trace-points))))
        (incf collected)
        (collect trace-point)))

(defun open-file-timeout (filename seconds &rest kwargs)
  "Try to open file, but give up after waiting SECONDS.
KWARGS are passed on to the OPEN call."
  (let ((start (get-internal-real-time))
        (thread (make-thread
                 (lambda ()
                   (handler-case
                       (apply #'cl:open filename #+ccl :sharing #+ccl :external
                              kwargs)
                     ;; Catch errors and return the condition to the main thread
                     (error (condition) condition))))))
    (iter (while (thread-alive-p thread))
          (let ((remaining (if (null seconds)
                               0.1
                               (- seconds
                                  (/ (- (get-internal-real-time) start)
                                     internal-time-units-per-second)))))
            (if (positive-real-p remaining)
                (sleep (min 0.1 remaining))
                (progn (destroy-thread thread)
                       (finish))))
          (finally
           (when-let ((result (handler-case
                                  (join-thread thread)
                                ;; Joining an aborted thread fails on SBCL
                                #+sbcl (sb-thread:join-thread-error () nil))))
             ;; Raise errors in the main thread
             (if (typep result 'error)
                 (error result)
                 (return result)))))))

(defmethod add-trace ((db sexp-trace-db) filename timeout metadata
                      &key max-trace-points)
  (when-let ((trace (read-sexp-trace filename
                                     :timeout timeout
                                     :max-trace-points max-trace-points)))
    (push trace (traces db))
    (push metadata (trace-metadata db))))

(defmethod add-trace-points ((db sexp-trace-db) (trace list)
                             &optional metadata)
  (push trace (traces db))
  (push metadata (trace-metadata db)))

(defmethod get-trace ((db sexp-trace-db) index &key file-id)
  (cons (cons :trace (if file-id
                         (remove-if-not [{eq file-id} #'cdr {assoc :f}]
                                        (nth index (traces db)))
                         (nth index (traces db))))
        (nth index (trace-metadata db))))

(defmethod get-trace ((db single-file-sexp-trace-db) index &key file-id)
  (declare (ignorable file-id))
  (cons (cons :trace (nth index (traces db)))
        (nth index (trace-metadata db))))

(defmethod n-traces ((db sexp-trace-db))
  (length (traces db)))

(defmethod trace-size ((db sexp-trace-db) index)
  (length (nth index (traces db))))

(defmethod trace-types ((db sexp-trace-db) index)
  (remove-duplicates (mapcar {elt _ 1}
                             (mappend [#'cdr {assoc :scopes}]
                                      (nth index (traces db))))
                     :test #'string=))

(defmethod query-trace ((db sexp-trace-db) index var-names var-types
                         &key pick file-id predicate soft-predicates
                           (filter (constantly t)))
  (declare (ignorable file-id var-names predicate))
  (assert (null soft-predicates)
          nil "SOFT-PREDICATES are not supported by SEXP-TRACE-DB")
  (labels ((satisfying-assignments-at-point (pt)
             (nest (mapcar (lambda (vars)
                             (list (assoc :c pt)
                                   (assoc :f pt)
                                   (assoc :aux pt)
                                   (cons :scopes vars))))
                   (cartesian-without-duplicates)
                   (mapcar (lambda (type-spec)
                             (remove-if-not (lambda (type)
                                              (member type type-spec
                                                      :test #'string=))
                                            (cdr (assoc :scopes pt))
                                            :key {elt _ 1}))
                           var-types)))
            (remove-duplicate-statement-and-bindings (query-results)
              (remove-duplicates query-results
                                 :key #'get-statement-and-bindings
                                 :test #'equalp)))
    (when-let* ((trace (nth index (traces db))))
      (cons (cons :results
                  (if pick
                      (nest (remove-duplicate-statement-and-bindings)
                            (remove-if-not (lambda (pt)
                                             (apply filter
                                                    (cdr (assoc :f pt))
                                                    (cdr (assoc :c pt))
                                                    (cdr (assoc :scopes pt)))))
                            (satisfying-assignments-at-point)
                            (random-elt trace))
                      (nest (remove-duplicate-statement-and-bindings)
                            (remove-if-not (lambda (pt)
                                             (apply filter
                                                    (cdr (assoc :f pt))
                                                    (cdr (assoc :c pt))
                                                    (cdr (assoc :scopes pt)))))
                            (mappend #'satisfying-assignments-at-point trace))))
            (nth index (trace-metadata db))))))

(defmethod restrict-to-file ((db sexp-trace-db) file-id)
  (make-instance 'single-file-sexp-trace-db :parent-db db :file-id file-id))

(defmethod trace-metadata ((db single-file-sexp-trace-db))
  (trace-metadata (parent-db db)))
