(defpackage :trace-db
  (:documentation "Wrapper for trace-db.")
  (:use :common-lisp
        :alexandria
        :iterate
        :cffi
        :trivial-garbage
        :named-readtables
        :curry-compose-reader-macros)
  (:export :read-trace
           :+trace-id-file-bits+
           :+trace-id-statement-bits+
           :trace-db
           :get-trace
           :add-trace
           :n-traces
           :trace-size
           :trace-types
           :query-trace
           :trace-metadata
           :distinct
           :v/value
           :v/size
           :set-trace))
