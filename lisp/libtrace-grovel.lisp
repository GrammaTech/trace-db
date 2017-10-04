(in-package :libtrace)

(cc-flags "-I/home/rswords/Projects/synthesis/sel/quicklisp/local-projects/trace-db/")

(include "types.h")
(include "read-trace.h")

(cenum type-format
  ((:unsigned "UNSIGNED"))
  ((:signed "SIGNED"))
  ((:float "FLOAT"))
  ((:pointer "POINTER"))
  ((:blob "BLOB"))
  ((:invalid-format "INVALID_FORMAT")))

(cstruct-and-class-item type-description "type_description"
  (name-index "name_index" :type :uint16)
  (format "format" :type type-format)
  (size "size" :type :uint8))

(cstruct trace-read-state "trace_read_state"
  (file "file" :type :pointer)
  (names "names" :type (:pointer :string))
  (n-names "n_names" :type :uint16)
  (types "types" :type (:pointer type-description))
  (n-types "n_types" :type :uint16))
#|
;; (cenum trace-entry-tag
;;   (:end-entry "END_ENTRY")
;;   (:statement-id "STATEMENT_ID")
;;   (:variable "VARIABLE")
;;   (:buffer-size "BUFFER_SIZE")
;;   (:auxiliary "AUXILIARY")
;;   (:trace-tag-error "TRACE_TAG_ERROR")
;;   (:end-of-trace "END_OF_TRACE"))

(cstruct-and-class-item buffer-size "trace_buffer_size" 
  (address "address" :type :uint64)
  (size "size" :type :uint64))



(cstruct-and-class-item var-info "trace_var_info"
  (value "value" :type :int64)
  (name-index "name_index" :type :uint16)
  (type-index "type_index" :type :uint16)
  (size "size" :type :uint16))



(cstruct-and-class-item trace-point "trace_point"
  (statement "statement" :type :uint32)
  (sizes "sizes" :type (:pointer buffer-size))
  (n-sizes "n_sizes" :type :uint32)
  (vars "vars" :type (:pointer var-info))
  (n-vars "n_vars" :type :uint32)
  (aux "aux" :type (:pointer :uint64))
  (n-aux "n_aux" :type :uint32))

|#
