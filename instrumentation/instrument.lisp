;;; instrument.lisp --- Generic interface for defining AST instrumentation.
(defpackage :trace-db/instrumentation/instrument
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/utility/task
        :software-evolution-library/software/project)
  (:export :instrument
           :uninstrument
           :instrumented-p
           :instrumenter
           :var-instrument
           :instrumentation-files))
(in-package :trace-db/instrumentation/instrument)
(in-readtable :curry-compose-reader-macros)


;;;; Instrumentation

(defgeneric instrumented-p (obj)
  (:documentation "Return true if OBJ is instrumented")
  (:method ((project project))
    (some #'instrumented-p (mapcar #'cdr (instrumentation-files project)))))

(defgeneric instrument (obj &key functions functions-after
                                 trace-file trace-env filter
                                 num-threads &allow-other-keys)
  (:documentation
   "Instrument OBJ to print AST index before each full statement.

The indices printed here are the position of the ast in (asts obj).

Keyword arguments are as follows:
  FUNCTIONS ------------ functions to calculate instrumentation at each point
  FUNCTIONS-AFTER ------ functions to calculate instrumentation after each point
  TRACE-FILE ----------- file for trace output
  TRACE-ENV ------------ trace output to file specified by ENV variable
  FILTER --------------- function to select a subset of ASTs for instrumentation
  NUM-THREADS ---------- number of threads to use for instrumentation"))

(defgeneric var-instrument (key instrumenter ast &key print-strings)
  (:documentation
   "Generate ASTs for variable instrumentation.
* KEY a function used to pull the variable list out of AST
* INSTRUMENTER current instrumentation state
* AST the AST to instrument"))

(defgeneric uninstrument (obj &key num-threads)
  (:documentation "Remove instrumentation from OBJ

Keyword arguments are as follows:
  NUM-THREADS ---------- number of threads to use for uninstrumenting")
  (:method ((project project) &key (num-threads 1))
    (task-map num-threads
              #'uninstrument
              (mapcar #'cdr (instrumentation-files project)))
    project))

(defgeneric instrumentation-files (project)
  (:documentation
   "Return files in PROJECT in the order which they would be instrumented")
  (:method ((project project))
    (evolve-files project)))

(defclass instrumenter ()
  ((software :accessor software :initarg :software :initform nil))
  (:documentation "Base class for objects which handle instrumentation.
Stores instrumentation state and provides methods for instrumentation
operations."))
