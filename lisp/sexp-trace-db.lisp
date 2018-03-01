;; sexp-trace-db.lisp - Trace database which uses a simple s-expression trace
;; format and LISP data structures.  This is a legacy trace format,
;; primarily for JAVA.  New clients should use the binary trace database
;; format.

(in-package :trace-db)
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

(defclass single-file-sexp-trace-db (sexp-trace-db) ())

(defun read-sexp-trace (file timeout &key (predicate #'identity) max)
  "Read a trace from FILE-NAME with `read-trace-stream'."
  (when-let ((in (open-file-timeout file timeout)))
    (unwind-protect
        (read-trace-stream in :max max :predicate predicate)
      (close in))))

(defun read-trace-stream
    (in &key (predicate #'identity) max &aux (collected 0))
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
                    (or (null max) (< collected max))))
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
          (let ((remaining (- seconds
                              (/ (- (get-internal-real-time) start)
                                 internal-time-units-per-second))))
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

(defmethod add-trace ((db sexp-trace-db) filename timeout metadata &key max)
  (when-let ((trace (read-sexp-trace filename timeout :max max)))
    (push trace (traces db))
    (push metadata (trace-metadata db))))

(defmethod set-trace ((db sexp-trace-db) index trace &optional metadata)
  (if (< index (n-traces db))
      (progn
        (setf (nth index (traces db)) trace)
        (setf (nth index (trace-metadata db)) metadata))
      (progn
        (appendf (traces db) trace)
        (appendf (trace-metadata db) metadata))))

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
  (remove-duplicates (mapcar #'second
                             (mappend [#'cdr {assoc :scopes}]
                                      (nth index (traces db))))
                     :test #'string=))

(defmethod query-trace ((db sexp-trace-db) index var-names var-types
                         &key pick file-id predicate filter)
  (declare (ignorable file-id var-names predicate))
  (labels ((satisfying-assignments-at-point (pt)
             (->> (mapcar (lambda (type-spec)
                            (remove-if-not (lambda (type)
                                             (member type type-spec
                                                     :test #'string=))
                                           (cdr (assoc :scopes pt))
                                           :key #'second))
                          var-types)
                  (cartesian-without-duplicates)
                  (mapcar (lambda (vars)
                            (->> (list (assoc :c pt)
                                       (assoc :f pt)
                                       (assoc :aux pt)
                                       (cons :scopes vars))
                                 (remove-if #'null)))))))
    (when-let* ((trace (nth index (traces db))))
      (if pick
          (->> (random-elt trace)
               (satisfying-assignments-at-point)
               (remove-if-not (lambda (pt)
                                (apply filter (cdr (assoc :c pt))
                                              (cdr (assoc :scopes pt))))))
          (->> (mappend #'satisfying-assignments-at-point trace)
               (remove-if-not (lambda (pt)
                                (apply filter (cdr (assoc :c pt))
                                              (cdr (assoc :scopes pt))))))))))

(defmethod restrict-to-file ((db sexp-trace-db) file-id)
  (make-instance 'single-file-sexp-trace-db
    :traces (mapcar (lambda (trace)
                      (remove-if-not [{eq file-id} #'cdr {assoc :f}] trace))
                    (traces db))
    :trace-metadata (trace-metadata db)))

(defun cartesian-without-duplicates (lists &key (test #'equalp))
  "Cartesian product of a set of lists, without sets containing duplicates.
For example:
> \(cartesian-without-duplicates '\(1 2\) '\(2 3\)\)
\(\(1 2\) \(1 3\) \(2 3\)\)
The set \(2 2\) is not included in the result.
"
  (labels ((cartesian-nil-duplicates (lists)
             (if (car lists)
                 (mappend (lambda (inner)
                            (mapcar (lambda (outer)
                                      (if (not (member outer inner :test test))
                                          (cons outer inner)
                                          nil))
                                    (car lists)))
                          (cartesian-nil-duplicates (cdr lists)))
                 (list nil))))
    (remove-if [{> (length lists)} #'length] (cartesian-nil-duplicates lists))))
