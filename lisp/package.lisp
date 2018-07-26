(defpackage :trace-db
  (:documentation "Wrapper for trace-db.")
  (:use :common-lisp
        :alexandria
        :bordeaux-threads
        :iterate
        :arrow-macros
        :cl-store
        :cffi
        :trivial-garbage
        :named-readtables
        :curry-compose-reader-macros)
  (:export :read-binary-trace
           :read-sexp-trace
           :+trace-id-file-bits+
           :+trace-id-statement-bits+
           :trace-db
           :binary-trace-db
           :sexp-trace-db
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
           :restrict-to-file
           :get-statement-and-bindings))
