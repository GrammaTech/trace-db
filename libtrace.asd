(defsystem :libtrace
  :description
  "Bug Injector: Inject Vulnerabilities for Configurable Cyber Defense"
  :version "0.0.0"
  :depends-on (alexandria
               iterate
               cffi
               cffi-libffi)
  :components
  ((:module lisp
            :pathname "lisp"
            :serial t
            :components
            ((:file "package")
             (:file "libtrace"))))
  :output-files (prepare-op (o c) (list "libtrace.so"))
  :perform (prepare-op :before (o c)
             (uiop::run-program
              (list "make" "-C"
                    (namestring
                     (asdf:component-pathname
                      (asdf:find-system :libtrace)))))))
