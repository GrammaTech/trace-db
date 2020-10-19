;;;; javascript.lisp --- Javascript instrumentation tests.
(defpackage :trace-db/test/javascript
  (:use
   :gt/full
   #+gt :testbot
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/javascript
   :software-evolution-library/software/javascript-project
   :software-evolution-library/components/test-suite
   :trace-db/instrumentation/instrument
   :trace-db/instrumentation/javascript-instrument
   :trace-db/traceable
   :trace-db/trace-db
   :trace-db/sexp-trace-db
   :trace-db/test/util)
  (:export :test-javascript))
(in-package :trace-db/test/javascript)
(in-readtable :curry-compose-reader-macros)

(defsuite test-javascript "Tests for Javascript instrumentation and tracing."
  (which "acorn"))

(define-software javascript-traceable
  (javascript sexp-traceable) ())
(define-software javascript-traceable-project
  (javascript-project sexp-traceable) ())

(defixture fib-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript-traceable)
                    (javascript-dir #P"fib/fib.js"))))
  (:teardown
   (setf *soft* nil)))

(defixture fib-project-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript-traceable-project
                      :component-class 'javascript-traceable)
                    (javascript-dir #P"fib-project/"))))
  (:teardown
   (setf *soft* nil)))

(deftest (javascript-instrument-and-collect-traces :long-running) ()
  (with-fixture fib-javascript
    (let ((instrumented (instrument *soft*)))
      (collect-traces instrumented
                      (make-instance 'test-suite
                        :test-cases (list (make-instance 'test-case
                                            :program-name "node"
                                            :program-args (list :bin)))))
      (is (equal 1 (n-traces (traces instrumented))))
      (is (equal '((:TRACE ((:C . 0))  ((:C . 37)) ((:C . 4))
                           ((:C . 13)) ((:C . 18)) ((:C . 22))
                           ((:C . 28)) ((:C . 32)) ((:C . 18))
                           ((:C . 22)) ((:C . 28)) ((:C . 32))
                           ((:C . 18)) ((:C . 22)) ((:C . 28))
                           ((:C . 32)) ((:C . 18)) ((:C . 22))
                           ((:C . 28)) ((:C . 32)) ((:C . 18))
                           ((:C . 22)) ((:C . 28)) ((:C . 32))
                           ((:C . 18)) ((:C . 22)) ((:C . 28))
                           ((:C . 32)) ((:C . 18)) ((:C . 22))
                           ((:C . 28)) ((:C . 32)) ((:C . 18))
                           ((:C . 22)) ((:C . 28)) ((:C . 32))
                           ((:C . 18)) ((:C . 22)) ((:C . 28))
                           ((:C . 32)) ((:C . 18)) ((:C . 22))
                           ((:C . 28)) ((:C . 32)) ((:C . 35)))
                   (:INPUT "node" :BIN))
                 (get-trace (traces instrumented) 0))))))

(deftest (javascript-instrument-and-collect-traces-with-vars :long-running) ()
  (with-fixture fib-javascript
    (let ((instrumented
           (instrument *soft*
                       :functions
                       (list (lambda (instrumenter ast)
                               (var-instrument
                                {get-vars-in-scope (software instrumenter)}
                                instrumenter
                                ast))))))
      (collect-traces instrumented
                      (make-instance 'test-suite
                        :test-cases (list (make-instance 'test-case
                                            :program-name "node"
                                            :program-args (list :bin)))))
      (is (equalp 1 (n-traces (traces instrumented))))
      (is (equalp '((:C . 22)(:SCOPES #("temp" "number" 1 nil)
                                      #("b" "number" 0 nil)
                                      #("a" "number" 1 nil)
                                      #("num" "number" 10 nil)))
                  (nth 5 (aget :trace (get-trace (traces instrumented) 0))))))))

(deftest (javascript-project-instrument-uninstrument-is-identity
          :long-running) ()
  (with-fixture fib-project-javascript
    (is (string= (genome-string *soft*)
                 (genome-string (uninstrument (instrument (copy *soft*))))))))

(deftest (javascript-project-instrument-and-collect-traces :long-running) ()
  (with-fixture fib-project-javascript
    (let ((instrumented (instrument *soft*)))
      (collect-traces instrumented
                      (nest (make-instance 'test-suite :test-cases)
                            (list (make-instance 'test-case
                                    :program-name (namestring
                                                   (javascript-dir
                                                    #P"fib-project/test.sh"))
                                    :program-args (list :bin "1")))))
      (is (equal 1 (n-traces (traces instrumented))))
      (is (equalp '(((:C . 0)  (:F . 1))
                    ((:C . 0)  (:F . 0))
                    ((:C . 6)  (:F . 1))
                    ((:C . 12) (:F . 1))
                    ((:C . 29) (:F . 1))
                    ((:C . 48) (:F . 1))
                    ((:C . 55) (:F . 1))
                    ((:C . 70) (:F . 1))
                    ((:C . 11) (:F . 0))
                    ((:C . 20) (:F . 0))
                    ((:C . 25) (:F . 0))
                    ((:C . 29) (:F . 0))
                    ((:C . 35) (:F . 0))
                    ((:C . 39) (:F . 0))
                    ((:C . 42) (:F . 0)))
                  (aget :trace (get-trace (traces instrumented) 0)))))))

(deftest (javascript-project-instrument-and-collect-traces-with-vars
          :long-running) ()
  (with-fixture fib-project-javascript
    (let ((instrumented
           (instrument *soft*
                       :functions
                       (list (lambda (instrumenter ast)
                               (var-instrument
                                {get-vars-in-scope (software instrumenter)}
                                instrumenter
                                ast))))))
      (collect-traces instrumented
                      (nest (make-instance 'test-suite :test-cases)
                            (list (make-instance 'test-case
                                    :program-name (namestring
                                                   (javascript-dir
                                                    #P"fib-project/test.sh"))
                                    :program-args (list :bin "1")))))
      (is (equal 1 (n-traces (traces instrumented))))
      (is (equalp '((:C . 29)(:F . 0)(:SCOPES #("temp" "number" 1 nil)
                                              #("b" "number" 0 nil)
                                              #("a" "number" 1 nil)
                                              #("num" "number" 1 nil)))
                  (nth 11 (aget :trace
                                (get-trace (traces instrumented) 0))))))))
