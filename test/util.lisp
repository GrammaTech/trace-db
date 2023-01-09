;;;; util.lisp --- Utils for trace-db tests.
(defpackage :trace-db/test/util
  (:use :gt/full
        :stefil+
        :software-evolution-library/software-evolution-library
        :software-evolution-library/software/cpp
        :software-evolution-library/software/c-cpp
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter
        :trace-db/core)
  (:export :test
           :*soft*
           :c-tree-sitter-available-p
           :cpp-tree-sitter-available-p
           :c/cpp-dir
           :stmt-with-text
           :stmt-starting-with-text))
(in-package :trace-db/test/util)
(in-readtable :curry-compose-reader-macros)

(defroot test)


;;; Variables and constants
(defvar *soft* nil "Software used in tests.")

(defconst +c-cpp-dir+
    (append +trace-db-dir+ (list "test" "etc" "c-cpp"))
  "Path to the directory holding c/cpp test artifacts.")


;;; Functions
(defun c-tree-sitter-available-p ()
  (handler-case (progn (make-instance 'sel/sw/tree-sitter::c))
    (error (e) (declare (ignorable e)) nil)))

(defun cpp-tree-sitter-available-p ()
  (handler-case (progn (make-instance 'sel/sw/tree-sitter::cpp))
    (error (e) (declare (ignorable e)) nil)))

(defun c/cpp-dir (path)
  "Return PATH relative to +c-cpp-dir+."
  (merge-pathnames-as-file (make-pathname :directory +c-cpp-dir+) path))

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
