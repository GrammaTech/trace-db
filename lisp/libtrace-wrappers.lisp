(in-package :libtrace)

(flag "-I/home/rswords/Projects/synthesis/sel/quicklisp/local-projects/trace-db/")
;(include "types.h")
;(include "read-trace.h")

(c "#include \"types.h\"")
(c "#include \"read-trace.h\"")

(defwrapper "start_reading" (:pointer trace-read-state)
  (filename :string)
  (timeout_seconds :int))

(defwrapper "end_reading" :void
  (state :pointer))

(defwrapper "read_trace_point" :int
  (state :pointer)
  (results :pointer))

;; (defwrapper "lseek" ("off_t" (errno-wrapper off))
;;   (fildes ("int" file-descriptor-designator))
;;   (offset ("off_t" off))
;;   (whence :int))

;; (defwrapper "mmap" ("void*" (errno-wrapper
;;                              :pointer
;;                              :error-predicate (lambda (p) (pointer-eq p *map-failed-pointer*))))
;;   (start :pointer)
;;   (length ("size_t" size))
;;   (prot :int)
;;   (flags :int)
;;   (fd ("int" file-descriptor-designator))
;;   (offset ("off_t" off)))

;; (defwrapper "start_reading" ("trace_read_state*")
;;   (filename :string)
;;   (timeout_seconds :int))
