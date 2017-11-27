(defpackage :trace-db
  (:documentation "Wrapper for trace-db.")
  (:use :common-lisp
        :alexandria
        :iterate
        :cffi)
  (:export :read-trace
           :+trace-id-file-bits+
           :+trace-id-statement-bits+
           :create-db
           :collect-trace
           :free-db
           :get-trace))
