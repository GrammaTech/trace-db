;; core.lisp --- Definition of trace db core constants
(defpackage :trace-db/core
  (:use :gt/full
        :software-evolution-library/utility/git)
  (:import-from :asdf :system-source-directory)
  (:export +trace-db-dir+ +trace-db-branch+))
(in-package :trace-db/core)
(in-readtable :curry-compose-reader-macros)

(defvar +trace-db-dir+
  (pathname-directory (system-source-directory 'trace-db))
  "Path to directory holding the TRACE-DB.")

(defvar +trace-db-branch+
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (handler-case
        (current-git-branch +trace-db-dir+)
      (git-error (e) (declare (ignorable e)) "UNKNOWN")))
  "Current branch of the TRACE-DB.")

