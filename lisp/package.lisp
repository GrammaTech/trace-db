(defpackage :libtrace
  (:documentation "Wrapper for libtrace.")
  (:use :common-lisp
        :alexandria
        :iterate
        :cffi)
  (:export :test-read
           :test-read-2
           :test-read-3))
