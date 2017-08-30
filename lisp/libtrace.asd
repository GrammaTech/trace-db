(defsystem :libtrace
  :description
  "Bug Injector: Inject Vulnerabilities for Configurable Cyber Defense"
  :version "0.0.0"
  :depends-on (alexandria
               iterate
               cffi
               cffi-libffi)
  :components ((:file "package")
               (:file "libtrace" :depends-on ("package"))))
