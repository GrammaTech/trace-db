(defpackage :libtrace
  (:documentation "Wrapper for libtrace.")
  (:use :common-lisp
        :alexandria
        :iterate
        :cffi)
  (:export :read-trace))
