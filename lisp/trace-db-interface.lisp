;; trace-db.lisp - Common interface for all trace databases.

(in-package :trace-db)
(in-readtable :curry-compose-reader-macros)

(defclass trace-db () ()
  (:documentation "Database containing dynamic traces."))

(defgeneric add-trace (db filename timeout metadata &key max)
  (:documentation "Read a trace from FILENAME into DB."))

(defgeneric get-trace (db index &key file-id)
  (:documentation "Retrieve the trace at INDEX in DB.

The result looks like: (METADATA (:trace (TRACE-POINTS))), where each
TRACE-POINT is an alist with the usual :C :F :SCOPES, etc."))

(defgeneric n-traces (db)
  (:documentation "Return the number of traces stored in DB."))

(defgeneric trace-size (db index)
  (:documentation "Return the number of points in the trace at INDEX in DB."))

(defgeneric trace-types (db index)
  (:documentation "Return the type names from the header of the trace at
INDEX in DB."))

(defgeneric trace-metadata (db)
  (:documentation "Return the metadata for all traces in DB."))

(defgeneric query-trace (db index var-names var-types
                         &key pick file-id predicate soft-predicates filter)
  (:documentation "Find bindings of VAR-NAMES in DB which satisfy PREDICATE.

VAR-TYPES is a list of lists containing the names of allowed types for
each variable.

If PREDICATE is given, only results which satisfy it are selected.

If SOFT-PREDICATES is given, then only results which satisfy the
maximum number of soft predicates at each trace point are selected.

Keyword arguments:
PICK ------------- return results from a single randomly-selected trace point
                   instead of searching all trace points.
FILE-ID ---------- restrict search to trace points in this file
PREDICATE -------- S-expression representing a database predicate
SOFT-PREDICATES -- Additional predicates to maximize
FILTER ----------- A function taking (FILE LOCATION VARS...) as arguments.
                   Results for which it returns false are discarded."))

(defgeneric restrict-to-file (db file-id)
  (:documentation "Return a wrapper around DB which restricts results by
FILE-ID."))

(defun get-statement-and-bindings (pt)
  "Return the file/statement counters and variable bindings for the given PT."
  (list (cdr (assoc :f pt))
        (cdr (assoc :c pt))
        (map 'list (lambda (var)
                     (elt var 0) (elt var 1))
                   (cdr (assoc :scopes pt)))))
