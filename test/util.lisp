;;;; util.lisp --- Utils for trace-db tests.
(defpackage :trace-db/test/util
  (:use :gt/full
        :stefil+
        :software-evolution-library/software-evolution-library
        :software-evolution-library/software/parseable
        :trace-db/core)
  (:export :test
           :*soft*
           :clang-dir
           :stmt-with-text
           :stmt-starting-with-text))
(in-package :trace-db/test/util)
(in-readtable :curry-compose-reader-macros)

(defroot test)


;;; Variables and constants
(defvar *soft* nil "Software used in tests.")

(defconst +clang-dir+
    (append +trace-db-dir+ (list "test" "etc" "clang"))
  "Path to the directory holding clang test artifacts.")


;;; Functions
(defun clang-dir (path)
  "Return PATH relative to +clang-dir+."
  (merge-pathnames-as-file (make-pathname :directory +clang-dir+)
                           path))

(defun stmt-with-text (obj text)
  "Return the AST in OBJ holding TEXT."
  (or (find-if [{string= text} #'source-text]
               (genome obj))
      (error "`stmt-with-text' failed to find ~S" text)))

(defun stmt-starting-with-text (obj text)
  "Return the AST in OBJ starting with TEXT."
  (or (find-if [{starts-with-subseq text} #'source-text]
               (genome obj))
      (error "`stmt-starting-with-text' failed to find ~S" text)))
