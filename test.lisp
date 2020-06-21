;;; test.lisp --- tests for the `trace-db' system
(defpackage :trace-db/test
  (:use :gt/full
        #+gt :testbot
        :software-evolution-library/stefil-plus
        :trace-db/core
        :trace-db/test/clang
        :trace-db/test/javascript
        :trace-db/test/util)
  (:export :test :batch-test :testbot-test))
(in-package :trace-db/test)
(in-readtable :curry-compose-reader-macros)

#-gt
(progn
  ;;; External replacement for GT-specific test submission helpers
  (defvar *success* nil "Variable indicating test success or failure.")
  (defun batch-test (test project branch &optional args)
    "Run tests in 'batch' mode, printing results as a string."
    (declare (ignorable project branch args))

    (let* ((stefil::*test-progress-print-right-margin* (expt 2 20))
           (failures (coerce (stefil::failure-descriptions-of
                              (without-debugging (funcall test)))
                             'list)))
      (setf *success*
            (if failures
                (prog1 nil
                  (format *error-output* "FAILURES~%")
                  (mapc [{format *error-output* "  ~a~%"}
                         #'stefil::name-of
                         #'stefil::test-of
                         #'car #'stefil::test-context-backtrace-of]
                        failures))
                (prog1 t
                  (format *error-output* "SUCCESS~%")))))))

(defun run-batch (&rest a)
  (declare (ignorable a))
  #+ccl (setf ccl::*interactive-streams-initialized* nil)
  (setf sel/stefil+:*long-tests* t)
  (batch-test #'test "SEL" +trace-db-branch+))
