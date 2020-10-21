;;; test.lisp --- tests for the `trace-db' system
(defpackage :trace-db/test
  (:use :gt/full
        #+gt :testbot
        :stefil+
        :trace-db/core
        :trace-db/test/clang
        :trace-db/test/javascript
        :trace-db/test/util)
  #+gt (:shadowing-import-from :testbot :batch-test)
  (:export :test :batch-test :testbot-test))
(in-package :trace-db/test)
(in-readtable :curry-compose-reader-macros)
