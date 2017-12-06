(defsystem :trace-db
  :description "Writing, reading, storing, and searching of program traces"
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
             (:file "trace-db"))))
  :output-files (prepare-op (o c) (list "libtrace-db.so"))
  :perform (prepare-op :before (o c)
             (uiop::run-program
              (list "make" "-C"
                    (namestring
                     (asdf:component-pathname
                      (asdf:find-system :trace-db)))))))
