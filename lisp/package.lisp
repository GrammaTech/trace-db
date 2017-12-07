(defpackage :trace-db
  (:documentation "Wrapper for trace-db.")
  (:use :common-lisp
        :alexandria
        :iterate
        :cffi
        :trivial-garbage)
  (:export :read-trace
           :+trace-id-file-bits+
           :+trace-id-statement-bits+
           :trace-db
           :get-trace
           :add-trace))
