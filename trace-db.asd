(defsystem "trace-db"
  :description "Writing, reading, storing, and searching of program traces"
  :version "0.0.0"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :in-order-to ((test-op (load-op "trace-db/test")))
  :perform (test-op (o c) (symbol-call :trace-db/test '#:run-batch)))
