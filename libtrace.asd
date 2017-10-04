(eval-when (:load-toplevel :execute)
  (operate 'load-op 'cffi-grovel))

(use-package 'cffi-grovel)

(defsystem :libtrace
  :description
  "Bug Injector: Inject Vulnerabilities for Configurable Cyber Defense"
  :version "0.0.0"
  :defsystem-depends-on (cffi-grovel)
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
             (:grovel-file "libtrace-grovel")
             (:wrapper-file "libtrace-wrappers" :soname "libtrace")
             (:file "libtrace"))))
  ;:output-files (prepare-op (o c) (list "libtrace.so"))
  ;; :perform (prepare-op :before (o c)
  ;;            (uiop::run-program
  ;;             (list "make" "-C"
  ;;                   (namestring
  ;;                    (asdf:component-pathname
  ;;                     (asdf:find-system :libtrace))))))
  )
