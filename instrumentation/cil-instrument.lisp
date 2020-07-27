;;; cil-instrument.lisp --- Instrument C Intermediate-language source files.
(defpackage :trace-db/instrumentation/cil-instrument
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/cil
        :trace-db/instrumentation/instrument))
(in-package :trace-db/instrumentation/cil-instrument)
(in-readtable :curry-compose-reader-macros)

(defmethod instrument ((cil cil) &key points functions functions-after
                                      trace-file trace-env target-asts
                                      instrument-exit filter num-threads)
  "Instrument CIL for traced execution.
Optionally specify the name of the file in which to save trace data as
`trace-file`."
  (unless (null trace-env)
    (warn "Tracing to env variable is not support for CIL software objects."))
  (unless (null points)
    (warn
     "Program point instrumentation not supported for CIL software objects."))
  (unless (and (null functions) (null functions-after))
    (warn
     "Custom function instrumentation not supported for CIL software objects."))
  (unless (null instrument-exit)
    (warn
     "Custom instrument-exit not supported for CIL software objects."))
  (unless (null filter)
    (warn
     "Custom filter not supported for CIL software objects."))
  (unless (null num-threads)
    (warn
     "Multi-threaded instrumented not supported for CIL software objects."))
  (unless (null target-asts)
    (warn
     "Instrumenting targeted asts is not supported for CIL software objects."))
  (apply-mutation cil (list :trace trace-file))
  cil)
