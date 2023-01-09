;;;; clang.lisp --- Clang instrumentation tests.
(defpackage :trace-db/test/clang
  (:use
   :gt/full
   #+gt :testbot
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/software/clang-project
   :software-evolution-library/components/test-suite
   :trace-db/instrumentation/instrument
   :trace-db/instrumentation/clang-instrument
   :trace-db/traceable
   :trace-db/trace-db
   :trace-db/binary-trace-db
   :trace-db/test/util)
  (:shadowing-import-from :uiop/run-program :run-program)
  (:export :test-clang))
(in-package :trace-db/test/clang)
(in-readtable :curry-compose-reader-macros)

(defsuite test-clang "Tests for clang instrumentation and tracing."
  (which "clang"))

(define-software clang-traceable (clang binary-traceable) ())

(defconst +gcd-inputs+
    '((:bin "1071" "1029")
      (:bin "555" "666")
      (:bin "678" "987")
      (:bin "8767" "653")
      (:bin "16777216" "512")
      (:bin "16" "4")
      (:bin "315" "831")
      (:bin "513332" "91583315")
      (:bin "112" "135")
      (:bin "310" "55"))
  "Example test inputs for GCD.")

(defvar *gcd-test-suite*
  (make 'test-suite
    :test-cases (iter (for input in +gcd-inputs+)
                      (collecting (make 'test-case
                                    :program-name (car input)
                                    :program-args (cdr input))))))

(defixture gcd-clang
  (:setup (setf *soft*
                (from-file (make 'clang)
                           (clang-dir #P"gcd/gcd.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture gcd-wo-curlies-clang
  (:setup (setf *soft*
                (from-file (make 'clang)
                           (clang-dir #P"gcd/gcd-wo-curlies.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture traceable-gcd
  (:setup (setf *soft*
                (from-file (make 'clang-traceable)
                           (clang-dir #P"gcd/gcd.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture c-strings
  (:setup
   (setf *soft*
         (from-file (make 'clang)
                    (clang-dir #P"strings/c-strings.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture cpp-strings
  (:setup
   (setf *soft*
         (from-file (make 'clang :compiler "clang++")
                    (clang-dir #P"strings/cpp-strings.cpp"))))
  (:teardown
   (setf *soft* nil)))

(defixture shadow-clang
  (:setup
   (setf *soft*
         (from-file (make 'clang)
                    (clang-dir #P"shadow/shadow.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture binary-search-clang
  (:setup
   (setf *soft*
         (from-file (make 'clang)
                    (clang-dir #P"binary-search/binary-search.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture clang-project
  (:setup
   (setf *soft*
         (from-file (make 'clang-project
                     :build-command "make foo"
                     :artifacts '("foo"))
                    (clang-dir #P"multi-file/"))))
  (:teardown
   (setf *soft* nil)))

(defixture grep-project
  (:setup
   (setf *soft*
         (from-file (make 'clang-project
                     :build-command "make grep"
                     :artifacts '("grep"))
                    (clang-dir #P"grep/"))))
  (:teardown
   (setf *soft* nil)))

(defixture print-env-clang
  (:setup (setf *soft*
                (from-file (make 'clang :compiler "clang")
                           (clang-dir #P"print-env/print-env.c"))))
  (:teardown (setf *soft* nil)))

(defixture long-running-program-clang
  (:setup
   (setf *soft*
         (from-file (make 'clang)
                    (clang-dir #P"long-running/long-running-program.c"))))
  (:teardown
   (setf *soft* nil)))

(defun do-multi-threaded-instrument-clang-test (obj)
  (let ((st-instrumented
         (instrument (copy obj)
                     :functions
                     (list (lambda (instrumenter ast)
                             (var-instrument {get-vars-in-scope
                                              (software instrumenter)}
                                             instrumenter
                                             ast)))
                     :num-threads 1))
        (mt-instrumented
         (instrument (copy obj)
                     :functions
                     (list (lambda (instrumenter ast)
                             (var-instrument {get-vars-in-scope
                                              (software instrumenter)}
                                             instrumenter
                                             ast)))
                     :num-threads 4)))
    (is (equalp (mapcar #'ast-class (asts st-instrumented))
                (mapcar #'ast-class (asts mt-instrumented)))
        "`instrument` should yield the same ASTs regardless of the ~
         number of threads utilized.")

    (uninstrument st-instrumented :num-threads 1)
    (uninstrument mt-instrumented :num-threads 4)
    (is (equalp (mapcar #'ast-class (asts st-instrumented))
                (mapcar #'ast-class (asts mt-instrumented)))
        "`uninstrument` should yield the same ASTs regardless of the ~
         number of threads utilized.")))

(deftest (multi-threaded-clang-instrument-test :long-running) ()
  (with-fixture clang-project
    (do-multi-threaded-instrument-clang-test *soft*))
  (with-fixture grep-project
    (do-multi-threaded-instrument-clang-test *soft*)))

(defun count-traceable (obj)
  "Return a count of full statements parented by compound statements"
  (count-if {can-be-made-traceable-p obj} (asts obj)))

(defun get-gcd-trace (bin)
  (with-temporary-file (:pathname trace-file)
    (let ((errno (nth-value 2 (run-program (format nil "~a 4 8 2>~a"
                                                   bin trace-file)))))
      (is (zerop errno))
      (let ((trace (read-binary-trace trace-file)))
        (is (listp trace))
        trace))))

(deftest instrumented-p-test ()
  (with-fixture gcd-clang
    (is (not (instrumented-p *soft*)))
    (is (instrumented-p (instrument (copy *soft*))))))

(deftest (instrumentation-insertion-test :long-running) ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *soft*) :trace-file :stderr)))
      ;; Do we insert the right number of printf statements?
      (is (<= (count-traceable *soft*)
              (count-traceable instrumented)))
      ;; Instrumented compiles and runs.
      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every {aget :c} trace))
          (is (= 15 (length trace))))))))

(deftest (instrumentation-insertion-w-filter-test :long-running) ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *soft*)
                                    :filter (lambda (obj ast)
                                              (eq 92 (index-of-ast obj ast)))
                                    :trace-file :stderr)))
      ;; Instrumented compiles and runs.
      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every {aget :c} trace))
          (is (= 1 (length trace))))))))

(deftest (instrumentation-insertion-w-points-test :long-running) ()
  (with-fixture gcd-clang
    (let ((instrumented
           (handler-bind ((warning #'muffle-warning))
             (instrument (copy *soft*)
                         :points
                         (iter (for ast in (stmt-asts *soft*))
                               (for i upfrom 0)
                               (collect (cons ast (if (evenp i) '(1 2) '(3 4) ))))
                         :trace-file :stderr))))
      (is (scan (quote-meta-chars "__write_trace_aux(__sel_trace_file")
                (genome-string instrumented))
          "We find code to print auxiliary values in the instrumented source.")
      ;; Instrumented compiles and runs.
      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every [«or {equalp #(1 2)} {equalp #(3 4)}»
                      {aget :aux}]
                     trace)))))))

(deftest (instrumentation-insertion-w-trace-file-test :long-running) ()
  (with-fixture gcd-clang
    (with-temporary-file (:pathname trace)
      (with-temporary-file (:pathname bin)
        (let ((instrumented
               (instrument (copy *soft*) :trace-file trace)))
          (is (scan (quote-meta-chars trace) (genome-string instrumented)))
          (is (zerop (nth-value 1 (ignore-phenome-errors
                                   (phenome instrumented :bin bin)))))
          (is (probe-file bin))
          (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
            (declare (ignorable stdout stderr))
            (is (zerop errno))
            (is (probe-file trace))))))))

(deftest (instrumentation-handles-missing-curlies-test :long-running) ()
  (with-fixture gcd-wo-curlies-clang
    (let ((instrumented (instrument (copy *soft*) :trace-file :stderr)))
      ;; Ensure we were able to instrument an else branch w/o curlies.
      (let* ((else-counter (index-of-ast *soft*
                                         (stmt-with-text *soft* "b = b - a;")))
             (matcher (format nil "__write_trace_id\\(.*~du\\)"
                              else-counter)))
        (is (scan matcher (genome-string instrumented)))
        ;; The next line (after flushing) should be the else branch.
        (let ((location (position-if {scan matcher} (lines instrumented))))
          (is (scan (quote-meta-chars "b = b - a;")
                    (nth location (lines instrumented))))))
      ;; Finally, lets be sure we still compile.
      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (is (not (emptyp (get-gcd-trace bin))))))))

(deftest
    (instrumentation-insertion-w-points-and-added-blocks-test :long-running) ()
  (with-fixture gcd-wo-curlies-clang
    (let* ((cookie 1234)
           (instrumented
            (instrument (copy *soft*)
                        :points
                        `((,(stmt-with-text *soft* "b - a") ,cookie))
                        :trace-file :stderr)))
      ;; Instrumented program holds the value 1234.
      (is (scan (quote-meta-chars (format nil "~d" cookie))
                (genome-string instrumented))
          "The point trace value ~S appears in the instrumented program text."
          cookie)
      ;; Instrumented compiles and runs.
      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (find-if [{equalp `#(,cookie)} {aget :aux}]
                       trace)
              "The point trace value ~S appears in the trace" cookie))))))

(deftest (instrumentation-after-insertion-mutation-test :long-running) ()
  "Ensure after applying an insert mutation, the instrumented software
prints unique counters in the trace"
  (with-fixture gcd-clang
    (let* ((*matching-free-var-retains-name-bias* 1.0)
           (variant (copy *soft*))
           (instrumented (copy *soft*))
           (stmt1 (stmt-with-text variant "a = atoi(argv[1]);"))
           (stmt2 (stmt-with-text variant "a = atoi(argv[1]);")))
      (apply-mutation variant
                      `(clang-insert (:stmt1 . ,stmt1) (:stmt2 . ,stmt2)))
      (instrument instrumented)

      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout stderr))
          (is (zerop errno))
          (is (not (equal (find-if [{string= "a = atoi(argv[1]);"}
                                    #'source-text]
                                   (asts variant)
                                   :from-end nil)
                          (find-if [{string= "a = atoi(argv[1]);"}
                                    #'source-text]
                                   (asts variant)
                                   :from-end t)))
              "a = atoi(argv[1]) was not inserted into the genome")
          (is (search
               (format
                nil
                "__write_trace_id(__sel_trace_file, __sel_trace_file_lock, ~du)"
                (nest (index-of-ast variant)
                      (find-if [{string= "a = atoi(argv[1]);"} #'source-text]
                               (asts variant)
                               :from-end nil)))
               (genome-string instrumented))
              "instrumentation was not added for the inserted statement")
          (is (search
               (format
                nil
                "__write_trace_id(__sel_trace_file, __sel_trace_file_lock, ~du)"
                (nest (index-of-ast variant)
                      (find-if [{string= "a = atoi(argv[1]);"} #'source-text]
                               (asts variant)
                               :from-end t)))
               (genome-string instrumented))
              "instrumentation was not added for the original statement"))))))

(deftest (instrumentation-print-unbound-vars :long-running) ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-unbound-vals
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_variables(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *soft* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (let ((trace (get-gcd-trace bin)))
        (is (= (length trace) (count-if {assoc :c} trace))
            "Counter in every trace element.")
        (is (> (count-if {assoc :scopes} trace) 0)
            "Variable list in some trace elements.")
        (is (> (length trace) (count-if {aget :scopes} trace))
            "Variable list not populated in every trace element.")))))

(deftest (instrumentation-print-in-scope-vars :long-running) ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_variables(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *soft* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (= (length trace) (count-if {assoc :c} trace))
            "Counter in every trace element.")
        (is (= (length trace) (count-if {assoc :SCOPES} trace))
            "Variable list in every trace element.")
        (is (= (length trace) (count-if {aget :SCOPES} trace))
            "Variable list populated in every trace element.")
        (is (not (null (mappend {aget :SCOPES} trace)))
            "Variable list not always empty.")))))

(defun trace-var-equal (var-name value scopes)
  (let ((var (find-if [{string= var-name} {aref _ 0}] scopes)))
    (or (null var)
        (equal (aref var 2) value))))

(deftest (instrumentation-print-strings :long-running) ()
  (with-fixture c-strings
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter ast
                                          :print-strings t)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_blobs(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print strings in the instrumented source.")
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *soft* :bin bin))))
          "Successfully compiled instrumented SOFT.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (every [{trace-var-equal "test" "test"} {aget :scopes}]
                   trace)
            "Variable 'test' always has expected value.")
        (is (every [{trace-var-equal "x" "4"} {aget :scopes}]
                   trace)
            "Variable 'x' always has expected value.")))))

(deftest (instrumentation-print-cpp-strings :long-running) ()
  (with-fixture cpp-strings
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter ast
                                          :print-strings t)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_blobs(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print strings in the instrumented source.")
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *soft* :bin bin))))
          "Successfully compiled instrumented SOFT.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (every [{trace-var-equal "x" "4"} {aget :scopes}]
                   trace)
            "Variable 'x' always has expected value.")))))

(deftest (instrumentation-print-vars-after :long-running) ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions-after
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_variables(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *soft* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (not (null (mappend {aget :SCOPES} trace)))
            "Variable list not always empty.")))))

(deftest (instrumentation-print-vars-handles-shadowing :long-running) ()
  (with-fixture shadow-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_variables(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *soft* :bin bin))))
          "Successfully compiled instrumented program.")
      (let ((trace (get-gcd-trace bin)))
        (without-compiler-notes
          (is (every [{eql 1} #'length {aget :scopes}]
                     trace)
              "No duplicate variables."))

        (is (every [«or {equalp '(#("x" "int" 1 nil))}
                        {equalp '(#("x" "short" 0 nil))}»
                    {aget :scopes}]
                   trace)
            "Variables have correct type and value.")))))

(deftest instrumentation-handles-binary-search ()
  (with-fixture binary-search-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-unbound-vals
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))))))

(deftest instrumentation-skips-nameless-variable ()
  (handler-bind ((mutate ; Ignore obvious problem in following genome.
                  (lambda (e)
                    (if (find-restart 'keep-partial-asts)
                        (invoke-restart 'keep-partial-asts)
                        (error e)))))
    (let ((soft (make 'clang
                 :genome "int test(int) { return 1; }")))
      (instrument soft :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter
                                          ast))))
      (is (not (scan (quote-meta-chars
                      "__write_trace_variables(__sel_trace_file")
                     (genome-string soft)))
          "No code to print variables in the instrumented source."))))

(deftest (instrumentation-preserves-annotations :long-running) ()
  (with-fixture gcd-clang
    (let* ((stmt (stmt-starting-with-text *soft* "if (a == 0)"))
           (index (index-of-ast *soft* stmt)))
      (apply-mutation *soft*
                      `(clang-replace
                        (:stmt1 . ,stmt)
                        (:value1 . ,(copy stmt :annotations '((:foo . t))))))

      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (when (aget :foo (ast-annotations ast))
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast))))
                  :trace-file :stderr)

      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome *soft* :bin bin))))
            "Successfully compiled instrumented GCD.")
        (is (member '(:foo . t)
                    (mappend #'ast-annotations (asts *soft*))
                    :test #'equalp))
        (let ((trace (get-gcd-trace bin)))
          (is (listp trace) "We got a trace.")
          (is (some {aget :scopes} trace))
          (is (every «or [#'not {aget :scopes}]
                         [{eq index} {aget :c}]»
                     trace)))))))

(deftest (uninstrument-instrument-is-identity :long-running) ()
  (with-fixture gcd-clang
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome-string orig)
                 (genome-string (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture gcd-wo-curlies-clang
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome-string orig)
                 (genome-string (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture clang-project
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome-string orig)
                 (genome-string (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture shadow-clang
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome-string orig)
                 (genome-string (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture binary-search-clang
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome-string orig)
                 (genome-string (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture c-strings
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome-string orig)
                 (genome-string (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture cpp-strings
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome-string orig)
                 (genome-string (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity"))))

(deftest (run-traceable-gcd :long-running) ()
  (with-fixture traceable-gcd
    (instrument *soft*)
    (collect-traces *soft* *gcd-test-suite*)
    (setf (traces *soft*)
          (mapcar {get-trace (traces *soft*)}
                  (iota (n-traces (traces *soft*)))))
    (is (= (length (traces *soft*)) (length +gcd-inputs+)))
    (is (every {every {aget :c}} (mapcar {aget :trace} (traces *soft*))))))

(deftest (run-traceable-gcd-w/collect-traces :long-running) ()
  (with-fixture traceable-gcd
    (instrument *soft*)
    (collect-traces *soft* *gcd-test-suite*)
    (setf (traces *soft*)
          (mapcar {get-trace (traces *soft*)}
                  (iota (n-traces (traces *soft*)))))
    (is (every {every {aget :c}} (mapcar {aget :trace} (traces *soft*))))))

(deftest (long-running-program-killed-test :long-running) ()
  (with-fixture long-running-program-clang
    (with-temporary-file (:pathname bin)
      (phenome *soft* :bin bin)
      (let ((proc (start-test bin
                              (make 'test-case :program-name bin)
                              :wait nil))
            (*process-kill-timeout* 4))
        (finish-test proc)
        (is (not (process-alive-p proc))
            "finish-test did not kill a long running process")))))

(deftest (env-variables-passed-through-to-test-suites :long-running) ()
  (with-fixture print-env-clang
    (with-temporary-file (:pathname bin)
      (phenome *soft* :bin bin)
      (is (string=
           (concatenate 'string "__sel_bar" '(#\Newline))
           (read-stream-content-into-string
            (process-info-output
             (start-test bin
                         (make 'test-case
                           :program-name bin
                           :program-args '("__sel_foo"))
                         :wait t
                         :output :stream
                         :env '(("__sel_foo" . "__sel_bar"))))))))))

(define-software collect-traces-handles-directory-phenomes-mock
    (parseable binary-traceable)
  ((phenome-dir :initarg phenome-dir :accessor phenome-dir :initform nil
                :copier :direct)))

(defmethod phenome ((obj collect-traces-handles-directory-phenomes-mock)
                    &key (bin (temp-file-name)))
  (let ((dir (ensure-directory-pathname bin)))
    (setf (phenome-dir obj) dir)
    (ensure-directories-exist dir)))

(deftest collect-traces-handles-directory-phenomes ()
  (let ((obj (make 'collect-traces-handles-directory-phenomes-mock)))
    (handler-bind ((trace-error (lambda (c)
                                  (declare (ignorable c))
                                  (invoke-restart 'ignore-empty-trace))))
      (collect-traces obj (make 'test-suite)))
    (is (not (probe-file (phenome-dir obj)))
        "collect-traces did not remove a phenome directory")))
