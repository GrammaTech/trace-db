(defpackage :libtrace
  (:documentation "Wrapper for libtrace.")
  (:use :common-lisp
        :alexandria
        :iterate
        :cffi)
  (:export :read-trace :libtrace))
(in-package :libtrace)

(define-constant +lib-dir+
    (make-pathname :directory (butlast (pathname-directory
                                        #.(or *compile-file-truename*
                                              *load-truename*
                                              *default-pathname-defaults*))))
  :test #'equalp
  :documentation "Path to directory holding shared library.")
