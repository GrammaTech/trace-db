;;; c-cpp.lisp --- Instrument C/CPP-language source files.
(defpackage :trace-db/instrumentation/c-cpp
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/utility/task
        :software-evolution-library/components/fodder-database
        :software-evolution-library/software/c
        :software-evolution-library/software/cpp
        :software-evolution-library/software/c-cpp
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/compilable
        :software-evolution-library/software/project
        :software-evolution-library/software/c-cpp-project
        :trace-db/binary-trace-db
        :trace-db/instrumentation/instrument
        :trace-db/traceable)
  (:import-from :functional-trees :path-later-p)
  (:export :c/cpp-instrumenter))
(in-package :trace-db/instrumentation/c-cpp)
(in-readtable :curry-compose-reader-macros)


;;; Instrumentation code
(defconst +instrument-log-lock-variable-name+ "__sel_trace_file_lock"
  "File lock variable used for instrumentation")

(defconst +trace-entry-macro-name+ "__GT_TRACEDB_ENTRY_POINT"
  "Name of the macro to be defined for entry points")

(defconst +write-trace-forward-declarations+
    (concatenate 'string "
#ifndef __GT_TRACEDB_FORWARD_DECLARATIONS
#define __GT_TRACEDB_FORWARD_DECLARATIONS

#include <stdarg.h>

enum __type_format {
    __GT_TRACEDB_UNSIGNED,       /* unsigned integer */
    __GT_TRACEDB_SIGNED,         /* signed integer */
    __GT_TRACEDB_FLOAT,          /* floating point */
    __GT_TRACEDB_POINTER,        /* unsigned, interpret as address */
    __GT_TRACEDB_BLOB,           /* arbitrary bytes, do not interpret */
    __GT_TRACEDB_INVALID_FORMAT
};

enum __trace_entry_tag {
    __GT_TRACEDB_END_ENTRY = 0,
    __GT_TRACEDB_STATEMENT_ID,
    __GT_TRACEDB_VARIABLE,
    __GT_TRACEDB_BUFFER_SIZE,
    __GT_TRACEDB_AUXILIARY,
    __GT_TRACEDB_TRACE_TAG_ERROR,
    /* Returned at EOF, should not appear in trace */
    __GT_TRACEDB_END_OF_TRACE
};

struct __trace_type_description;
struct __trace_buffer_size;
typedef struct __trace_type_description __trace_type_description;
typedef struct __trace_buffer_size __trace_buffer_size;

__attribute__((unused))
extern void * " +trace-instrument-log-variable-name+ ";
__attribute__((unused))
extern void * " +instrument-log-lock-variable-name+ ";

__attribute__((unused))
static void __write_trace_header(void *out, void *lock,
                                 const char **names,
                                 const unsigned long n_names,
                                 const __trace_type_description *types,
                                 const unsigned long n_types);

__attribute__((unused))
static void __write_trace_id(void *out,
                             void *lock,
                             const unsigned long long statement_id);

__attribute__((unused))
static void __write_end_entry(void *out, void *lock);

__attribute__((unused))
static void __write_trace_variables(void *out, const unsigned long n_vars, ...);

__attribute__((unused))
static void __write_trace_blobs(void *out, const unsigned long n_vars, ...);

__attribute__((unused))
static void __write_buffer_size(void *out,
                                void *address,
                                const unsigned long long size);

#endif

")
  "C code to include in all instrumented files.")


(defconst +write-trace-implementation+
    "
#ifndef __GT_TRACEDB_IMPLEMENTATION
#define __GT_TRACEDB_IMPLEMENTATION

#include <pthread.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

struct __trace_type_description {
    /* Index into the string dictionary which gives the name of the type. */
    uint32_t name_index;
    /* Data format */
    enum __type_format format;
    /* Size in bytes. 0 indicates a variable-sized object. */
    uint32_t size;
};

struct __trace_buffer_size {
    uint64_t address;
    uint64_t size;
};

static void __write_trace_header(void *out, void *lock,
                                 const char **names,
                                 const unsigned long n_names,
                                 const __trace_type_description *types,
                                 const unsigned long n_types)
{
    /* Dictionary of names, as a sequence of NULL-terminated strings */
    uint64_t total_size = 0;
    uint32_t i = 0;

    /* Write trace header as single transaction */
    pthread_mutex_lock( (pthread_mutex_t *) lock);

    for (i = 0; i < n_names; i++) {
        total_size += strlen(names[i]) + 1;
    }
    fwrite(&total_size, sizeof(uint64_t), 1, (FILE *) out);

    for (i = 0; i < n_names; i++) {
        fputs(names[i], (FILE *) out);
        fputc(0, (FILE *) out);
    }

    /* Dictionary of types */
    fwrite(&n_types, sizeof(uint32_t), 1, (FILE *) out);
    fwrite(types, sizeof(__trace_type_description), n_types, (FILE *) out);

    /* Finished writing trace header */
    pthread_mutex_unlock( (pthread_mutex_t *) lock);
}

static void __write_trace_id(void *out,
                             void *lock,
                             const unsigned long long statement_id)
{
    /* Write trace point as single transaction */
    pthread_mutex_lock( (pthread_mutex_t *) lock);

    fputc(__GT_TRACEDB_STATEMENT_ID, (FILE *) out);
    fwrite(&statement_id, sizeof(uint64_t), 1, (FILE *) out);
}

static void __write_end_entry(void *out, void *lock)
{
    fputc(__GT_TRACEDB_END_ENTRY, (FILE *) out);
    fflush( (FILE *) out);

    /* Finished writing trace point */
    pthread_mutex_unlock( (pthread_mutex_t *) lock);
}

static void __write_trace_variables(void *out, const unsigned long n_vars, ...)
{
    va_list ap;
    uint32_t i;

    va_start (ap, n_vars);
    for (i = 0; i < n_vars; i++) {
        uint32_t name_index = va_arg(ap, uint32_t);
        uint32_t type_index = va_arg(ap, uint32_t);
        uint32_t size = va_arg(ap, uint32_t);
        enum __type_format format = (enum __type_format) va_arg(ap, int);

        fputc(__GT_TRACEDB_VARIABLE, (FILE *) out);
        fwrite(&name_index, sizeof(uint32_t), 1, (FILE *) out);
        fwrite(&type_index, sizeof(uint32_t), 1, (FILE *) out);

        /* This code is tricky because va_args are subject to standard
         promotions: smaller integers become ints, and floats become
         doubles. Other types are left alone.
        */
        switch (format) {
        case __GT_TRACEDB_UNSIGNED:
        case __GT_TRACEDB_SIGNED:
          switch (size) {
          case 1:
              {
                  char val = va_arg(ap, int);
                  fwrite(&val, sizeof(char), 1, (FILE *) out);
                  break;
              }
          case 2:
              {
                  int16_t val = va_arg(ap, int);
                  fwrite(&val, sizeof(int16_t), 1, (FILE *) out);
                  break;
              }
          case 4:
              {
                  int32_t val = va_arg(ap, int);
                  fwrite(&val, sizeof(int32_t), 1, (FILE *) out);
                  break;
              }
          case 8:
              {
                  int64_t val = va_arg(ap, int64_t);
                  fwrite(&val, sizeof(int64_t), 1, (FILE *) out);
                  break;
              }
          }
          break;
        case __GT_TRACEDB_FLOAT:
          if (size == 4) {
              float val = (float)va_arg(ap, double);
              fwrite(&val, sizeof(float), 1, (FILE *) out);
              break;
          }
          else {
              double val = va_arg(ap, double);
              fwrite(&val, sizeof(double), 1, (FILE *) out);
              break;
          }
          break;
        case __GT_TRACEDB_POINTER:
            {
                void *val = va_arg(ap, void*);
                fwrite(&val, sizeof(void*), 1, (FILE *) out);
            }
            break;
        case __GT_TRACEDB_BLOB:
        case __GT_TRACEDB_INVALID_FORMAT:
        default:
          break;
        }
    }
}

static void __write_trace_blobs(void *out, const unsigned long n_vars, ...)
{
    va_list ap;
    uint32_t i;

    va_start (ap, n_vars);
    for (i = 0; i < n_vars; i++) {
        uint32_t name_index = va_arg(ap, uint32_t);
        uint32_t type_index = va_arg(ap, uint32_t);
        uint32_t size = va_arg(ap, uint32_t);
        void *value = va_arg(ap, void*);

        fputc(__GT_TRACEDB_VARIABLE, (FILE *) out);
        fwrite(&name_index, sizeof(uint32_t), 1, (FILE *) out);
        fwrite(&type_index, sizeof(uint32_t), 1, (FILE *) out);
        fwrite(&size, sizeof(uint32_t), 1, (FILE *) out);
        fwrite(value, size, 1, (FILE *) out);
    }
}

static void __write_buffer_size(void *out,
                                void *address,
                                const unsigned long long size)
{
    fputc(__GT_TRACEDB_BUFFER_SIZE, (FILE *) out);
    __trace_buffer_size val = { (uint64_t)address, (uint64_t)size };
    fwrite(&val, sizeof(val), 1, (FILE *) out);
}

#endif
"
  "C code which implements trace writing.")

(defconst +names-variable-name+ "names"
  "Name of the variable containing instrumentation var names.")

(defconst +types-variable-name+ "types"
  "Name of the variable containing instrumentation types.")

(defconst +write-trace-entry-macro+
    (concatenate 'string "
#ifndef " +trace-entry-macro-name+ "
#define " +trace-entry-macro-name+ "
#endif
")
  "C code which defines a macro for trace database entry points.")

(defconst +write-trace-initialization+
    (concatenate 'string "
#ifdef " +trace-entry-macro-name+ "

#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

void * " +trace-instrument-log-variable-name+ ";
void * " +instrument-log-lock-variable-name+ ";

void __attribute__((constructor(101))) __bi_setup() {
  static pthread_mutex_t __static_lock;
  FILE *handshake_file = NULL;
  const char *handshake_file_path = getenv(\"~a\");
  char buffer[1024] = \"/dev/null\";

  if (handshake_file_path) {
    while (access(handshake_file_path, 0) != 0) { sleep(1); }
    handshake_file = fopen(handshake_file_path, \"r\");
    do { sleep(1); } while (fgets(buffer, 1024, handshake_file) == NULL);
    buffer[strcspn(buffer, \"\\n\")] = 0;
    fclose(handshake_file);
    unlink(handshake_file_path);
  }

  " +trace-instrument-log-variable-name+ " = ~a;
  " +instrument-log-lock-variable-name+ " = &__static_lock;
  ~a;
  ~a;

  pthread_mutex_init( (pthread_mutex_t *) " +instrument-log-lock-variable-name+
  ", NULL);
  __write_trace_header(" +trace-instrument-log-variable-name+ ",
                       " +instrument-log-lock-variable-name+ ",
                       " +names-variable-name+ ", ~d,
                       " +types-variable-name+ ", ~d);
}

#endif
")
  "C code which initializes the trace file")

(defconst +traceable-stmt-types+
  '(statement-ast
    c/cpp-preproc-if
    c/cpp-preproc-ifdef
    c/cpp-preproc-def
    c/cpp-preproc-include)
  "Types of ASTs which may be instrumented/traced.")

(defconst +traceable-parent-types+
  '(compound-ast
    c/cpp-preproc-if
    c/cpp-preproc-ifdef
    c/cpp-preproc-elif
    c/cpp-preproc-else
    c/cpp-case-statement
    c/cpp-labeled-statement)
  "Parent types of ASTs which may be instrumented/traced.")

(defconst +error-ast-types+
  '(variation-point
    parse-error-ast
    source-text-fragment)
  "Types of error AST types.")


;;; Data structures
(defclass c/cpp-instrumenter (instrumenter)
  ((names :accessor names
          :initarg :names
          :initform (make-hash-table :test #'equal)
          :documentation "Mapping of names to indices.")
   (types :accessor types
          :initarg :types
          :initform (make-hash-table :test #'equal)
          :documentation "Mapping of trace type strings to indices.")
   (type-descriptions :accessor type-descriptions
                      :initarg :type-descriptions
                      :initform (make-hash-table :test #'equal)
                      :documentation "Mapping of type descriptions to indices.")
   (ast-ids :accessor ast-ids
            :initarg :ast-ids
            :initform nil
            :documentation "Mapping of ASTs to trace ids."))
  (:documentation "Handles instrumentation for c/cpp software objects."))

(-> get-ast-id (c/cpp-instrumenter ast)
               (values t boolean &optional))
(defun get-ast-id (instrumenter ast)
  "Return the trace ID for AST
* INSTRUMENTER current instrumentation state
* AST the AST to instrument"
  (gethash ast (ast-ids instrumenter)))

(-> get-ast-ids-ht (c/cpp &optional (or number null))
                   (values hash-table &optional))
(defun get-ast-ids-ht (obj &optional file-id)
  "Return a hash table mapping AST index to
trace statement ID for each AST in OBJ.
* OBJ C/CPP software object
* FILE-ID index of OBJ in a project"
  (let ((ht (make-hash-table :test #'eq)))
    (iter (for ast in (asts obj))
          (for ast-i upfrom 0)
          (setf (gethash ast ht)
                (if file-id
                    (logior (ash 1 63)                              ; flag bit
                            (ash file-id +trace-id-statement-bits+) ; file ID
                            ast-i)                                  ; AST ID
                    ast-i))
          (finally (return ht)))))


;;; Instrumentation
(defmethod instrumented-p ((obj c/cpp))
  "Return true if OBJ is instrumented
* C/CPP a C or CPP software object"
  (search +trace-instrument-log-variable-name+ (genome-string obj)))

(defmethod instrument ((obj c/cpp) &rest args)
  "Instrumentation for C/CPP software objects.
Creates a C/CPP-INSTRUMENTER for OBJ and calls its instrument method.

* OBJ the software object to instrument
* ARGS additional arguments are passed through to the instrumenter method."
  (apply #'instrument
         (make 'c/cpp-instrumenter
           :software obj
           :ast-ids (get-ast-ids-ht obj))
         args))

(defmethod instrument
    ((instrumenter c/cpp-instrumenter)
     &key functions functions-after trace-file trace-env
       (filter (constantly t)) (num-threads 0)
     &aux (obj (software instrumenter))
       (root (genome obj)))
  "Use INSTRUMENTER to instrument a C/CPP software object.

* INSTRUMENTER current instrumentation state
* FUNCTIONS  functions to calculate instrumentation at each point
* FUNCTIONS-AFTER functions to calculate instrumentation after each point
* TRACE-FILE file or stream (stdout/stderr) for trace output
* TRACE-ENV trace output to file specified by ENV variable
* FILTER function to select a subset of ASTs for instrumentation
         function should take a software object and an AST parameters,
         returning nil if the AST should be filtered from instrumentation
* NUM-THREADS number of threads to use for instrumentation"
  (declare (ignorable num-threads))
  (labels
      ((sort-asts (obj asts)
         "Sort ASTs in OBJ for instrumentation."
         (sort asts {path-later-p obj}))
       (instrument-ast (obj ast extra-stmts extra-stmts-after)
         "Generate instrumentation for the AST. Returns a list of
          (AST-PATH AST INSTRUMENTATION-BEFORE INSTRUMENTATION-AFTER)."
         `(,(ast-path obj ast)
           ,ast
           ;; Instrumentation before
           (;; Instrument before
            ,(write-trace-id instrumenter ast)
            ,@extra-stmts
            ,(write-end-entry instrumenter))
           ;; Instrumentation after
           (,@(when functions-after
                `(,(write-trace-id instrumenter ast)
                  ,@extra-stmts-after
                  ,(write-end-entry instrumenter))))))
       (instrument-asts (obj)
         "Generate instrumentation for all ASTs in OBJ."
         (nest (mapcar
                 (lambda (ast)
                   (instrument-ast obj
                                   ast
                                   (mappend {funcall _ instrumenter ast}
                                            functions)
                                   (mappend {funcall _ instrumenter ast}
                                            functions-after))))
               (sort-asts obj)
               (remove-if-not {funcall filter obj})
               (remove-if-not {can-be-made-traceable-p obj})
               (asts obj)))
       (wrap-in-compound-stmt-if-needed (obj ast new-ast)
         "Wrap NEW-AST in a compound statement for instrumentation if needed."
         (if (not (traceable-stmt-p obj ast))
             (convert (language-ast-class obj)
                      `((:class . :compound-statement)
                        (:children . ,(list new-ast))
                        (:annotations . ((:instrumentation . t)))))
             new-ast)))

    ;; Apply mutations to instrument OBJ.
    (iter (for (ast-path ast before after) in (instrument-asts obj))
          (setf root
                (nest (with root ast-path)
                      (wrap-in-compound-stmt-if-needed obj ast)
                      (wrap-before-after-asts (lookup root ast-path)
                                              before
                                              after)))
          (finally (setf (genome obj) root)))

    ;; Add support code for tracing to obj
    (prepend-instrumentation-setup-code obj +write-trace-forward-declarations+)
    (append-instrumentation-setup-code obj +write-trace-implementation+)
    (initialize-tracing instrumenter trace-file trace-env (main-declarators obj))

    ;; Add flag to allow building with pthreads
    (appendf (flags obj) (list "-lpthread"))

    obj))

(defmethod uninstrument ((obj c/cpp) &key (num-threads 0))
  "Remove instrumentation from OBJ."
  (declare (ignorable num-threads))

  ;; Remove instrumentation ASTs - blocks first, then individual statements.
  (nest (apply-mutation-ops obj)
        (mapcar (lambda (ast)
                  `(:set (:stmt1 . ,ast)
                         (:value1 .
                          ,{lookup obj
                                   (nest (ast-path obj)
                                         (first)
                                         (direct-children)
                                         (lookup obj)
                                         (ast-path obj ast))}))))
        (reverse)
        (remove-if-not (of-type 'compound-ast))
        (remove-if-not #'instrumentation-ast-p)
        (asts obj))

  (nest (apply-mutation-ops obj)
        (mapcar (lambda (ast)
                  `(:cut (:stmt1 . ,ast))))
        (reverse)
        (remove-if-not #'instrumentation-ast-p)
        (asts obj)))

(defmethod instrumentation-files ((c/cpp-project c/cpp-project))
  "Return files in C/CPP-PROJECT in the order which they would be instrumented
* C/CPP-PROJECT a project to instrument"
  (append (remove-if #'contains-main-p
                     (evolve-files c/cpp-project)
                     :key #'cdr)
          (remove-if [#'not #'contains-main-p]
                     (append (evolve-files c/cpp-project)
                             (other-files c/cpp-project))
                     :key #'cdr)))

(defmethod instrument ((c/cpp-project c/cpp-project) &rest args
    &aux (names (make-thread-safe-hash-table :test #'equalp))
         (types (make-thread-safe-hash-table :test #'equalp))
         (type-descriptions (make-thread-safe-hash-table :test #'equalp))
         (files (instrumentation-files c/cpp-project))
         (num-threads (or (plist-get :num-threads args) 0)))
  "Instrument C/CPP-PROJECT to print AST index before each full statement.

* C/CPP-PROJECT the project to instrument
* ARGS passed through to the instrument method on underlying software objects."
  ;; Instrument the non-entry point files in the project in parallel.
  (task-map num-threads
            (lambda (instrumenter)
              (apply #'instrument instrumenter args))
            (iter (for obj in (remove-if #'contains-main-p (mapcar #'cdr files)))
                  (for file-id upfrom 0)
                  (collect (make 'c/cpp-instrumenter
                             :software obj
                             :names names
                             :types types
                             :type-descriptions type-descriptions
                             :ast-ids (get-ast-ids-ht obj file-id)))))

  ;; Instrument the evolve-files with entry points to the program.
  ;; We do this after instrumenting all non-entry point files
  ;; to ensure the names, types, and type-description hash tables
  ;; are complete.
  (iter (for (path . obj) in files)
        (for file-id upfrom 0)
        (declare (ignorable path))
        (when (contains-main-p obj)
          (apply #'instrument
                 (make 'c/cpp-instrumenter
                   :software obj
                   :names names
                   :types types
                   :type-descriptions type-descriptions
                   :ast-ids (get-ast-ids-ht obj file-id))
                 args)))

  ;; Insert log setup code in other-files with an entry point.
  (iter (for obj in (mapcar #'cdr (other-files c/cpp-project)))
        (when-let ((entry-points (main-declarators obj)))
          (prepend-instrumentation-setup-code obj +write-trace-forward-declarations+)
          (append-instrumentation-setup-code obj +write-trace-implementation+)
          (initialize-tracing (make 'c/cpp-instrumenter
                                :software obj
                                :names names
                                :types types
                                :type-descriptions type-descriptions)
                              (plist-get :trace-file args)
                              (plist-get :trace-env args)
                              entry-points)))

  c/cpp-project)


;;; Tracing functions
(defgeneric enclosing-traceable-stmt (software ast)
  (:documentation "Return the first ancestor of AST in SOFTWARE which may
be a full stmt.  If a statement is reached which is not itself traceable,
but which could be made traceable by wrapping with curly braces, return that.")
  (:method ((obj c/cpp) (ast c/cpp-ast))
    (cond
      ((traceable-stmt-p obj ast) ast)
      ;; Wrap AST in a CompoundStmt to make it traceable.
      ((can-be-made-traceable-p obj ast) ast)
      (:otherwise
       (when-let ((parent (get-parent-ast obj ast)))
         (enclosing-traceable-stmt obj parent))))))

(defmethod can-be-made-traceable-p ((obj c/cpp) (ast ast))
  (or (traceable-stmt-p obj ast)
      ;; Is a child of a statement which might have a hanging body.
      (and (not (typep ast 'compound-ast))
           (and-let* ((parent (get-parent-ast obj ast)))
             (cond ((typep parent 'loop-ast)
                    (eq ast (c/cpp-body parent)))
                   ((typep parent 'c/cpp-if-statement)
                    (or (eq ast (c/cpp-alternative parent))
                        (eq ast (c/cpp-consequence parent))))
                   (:otherwise nil)))
           (not (possibly-incomplete-ast-p obj ast)))))

(defmethod traceable-stmt-p ((obj c/cpp) (ast ast))
  (and (traceable-stmt-ast-type-p ast)
       (traceable-stmt-parent-ast-type-p obj ast)
       (not (possibly-incomplete-ast-p obj ast))))

(-> traceable-stmt-ast-type-p (ast) (values list &optional))
(defun traceable-stmt-ast-type-p (ast)
  "Return non-NIL if AST is of a type which may be traced."
  (and (null (member ast +traceable-parent-types+ :test #'typep))
       (member ast +traceable-stmt-types+ :test #'typep)))

(-> traceable-stmt-parent-ast-type-p (c/cpp ast) (values boolean &optional))
(defun traceable-stmt-parent-ast-type-p (obj ast)
  "Return non-NIL if the parent of AST in OBJ is of a type which may have
traceable children."
  (let* ((parents (get-parent-asts* obj ast))
         (enclosing-function (find-if (of-type 'c/cpp-function-definition)
                                      parents)))
    (and (member (car parents) +traceable-parent-types+ :test #'typep)
         (some (of-type 'compound-ast) parents)
         (not (member (when enclosing-function (function-name enclosing-function))
                      '("struct" "enum" "class")
                      :test #'equal)))))

(-> possibly-incomplete-ast-p (c/cpp ast) (values boolean &optional))
(defun possibly-incomplete-ast-p (obj ast &aux (root (genome obj)))
  "Return non-NIL if AST may be preceeded or succeeded by additional,
partial ASTs which add information used at compile time.

For instance, in the following example:

#ifdef __STDC__
const
#endif
int foo;

`int foo` is incomplete as the previous macro adds an additional qualifier
if __STDC__ is defined."
  (labels ((variation-point-ast-p (ast)
             (typep ast 'variation-point))
           (possibly-incomplete-macro-p (ast)
             (and (or (typep ast 'c/cpp-preproc-if)
                      (typep ast 'c/cpp-preproc-ifdef))
                  (variation-point-ast-p (lastcar (butlast (children ast))))))
           (possibly-incomplete-ast-helper (ast)
             (or (variation-point-ast-p ast)
                 (possibly-incomplete-macro-p ast))))
    (or (possibly-incomplete-ast-helper (previous-sibling root ast))
        (possibly-incomplete-ast-helper (next-sibling root ast)))))

(defun previous-sibling (root ast)
  "Return the previous sibling of AST in ROOT."
  (lastcar (find-preceding (constantly t) root ast)))

(defun next-sibling (root ast)
  "Return the next sibling of AST in ROOT."
  (car (find-following (constantly t) root ast)))


;;; Types tracing functions
(defconst +c/cpp-type-qualifiers+
  '("const" "volatile" "restrict"))

(defconst +c/cpp-type-storage-classes+
  '("extern" "static" "auto" "register"))

(defconst +c/cpp-type-modifiers+
  '("signed" "unsigned" "short" "long"))

(defconst +c/cpp-primitive-types+
  '("char" "int" "float" "double"))

(defparameter +c/cpp-array-regex+
  (create-scanner "\\[[^\\[\\]]*\\]"))

(defparameter +c/cpp-array-or-pointer-regex+
  (create-scanner "^(\\*|\\[[^\\[\\]]*\\])+"))

(defparameter +integer-regex+
  (create-scanner "^[0-9]+$"))

(defparameter *type-from-trace-string-cache*
  (make-thread-safe-hash-table :test #'equal))

(defmethod type-trace-string ((canonical-type c/cpp-canonical-type))
  (labels ((pointer-str ()
             (nest (apply #'concatenate 'string)
                   (repeatedly (count-if [{eq :pointer} #'car]
                                         (declarator canonical-type))
                               "*")))
           (array-str ()
             (nest (apply #'concatenate 'string)
                   (mapcar (lambda (array-spec)
                             (if (nest (scan +integer-regex+)
                                       (source-text)
                                       (second array-spec))
                                 (fmt "[~d]" (source-text (second array-spec)))
                                 "[]")))
                   (remove-if-not [{eq :array} #'car])
                   (declarator canonical-type)))
           (add-declators-to-first-specifier ()
             (fmt "~a~a~a"
                  (pointer-str)
                  (array-str)
                  (source-text (car (specifier canonical-type))))))

    (nest (fmt "~{~a~^ ~}")
          (cons (add-declators-to-first-specifier))
          (mapcar #'source-text)
          (cdr)
          (specifier canonical-type))))

(defmethod type-from-trace-string ((trace-string string)
                                   (language (eql :c)))
  (type-from-trace-string-common trace-string language))

(defmethod type-from-trace-string ((trace-string string)
                                   (language (eql :cpp)))
  (type-from-trace-string-common trace-string language))

(-> type-from-trace-string-common (string keyword)
                                  (values c/cpp-canonical-type &optional))
(defun type-from-trace-string-common
    (type-string language &aux (split (split-sequence #\Space type-string)))
  (labels ((create-ast (clazz text)
             (convert (if (eql language :c) 'c-ast 'cpp-ast)
                      `((:class . ,clazz)
                        (:text . ,text))))
           (strip-array-or-pointer (str)
             (regex-replace-all +c/cpp-array-or-pointer-regex+ str ""))
           (pointer-declarator-component ()
             (repeatedly (count #\* type-string) '(:pointer)))
           (array-declarator-component ()
             (mapcar (lambda (str)
                       (let ((size (subseq str 1 (1- (length str)))))
                         (cond ((emptyp size)
                                (list :array))
                               ((scan +integer-regex+ size)
                                (list :array (create-ast :number-literal size)))
                               (:otherwise
                                (list :array (create-ast :identifier size))))))
                     (all-matches-as-strings +c/cpp-array-regex+ type-string)))
           (type-qualifiers ()
             (nest (mapcar {create-ast :type-qualifier})
                   (remove-if-not {member _ +c/cpp-type-qualifiers+
                                          :test #'equal})
                   (mapcar #'strip-array-or-pointer split)))
           (type-storage-classes ()
             (nest (mapcar {create-ast :storage-class-specifier})
                   (remove-if-not {member _ +c/cpp-type-storage-classes+
                                          :test #'equal})
                   (mapcar #'strip-array-or-pointer split)))
           (base-type ()
             (nest (mapcar (lambda (str)
                             (cond ((member str +c/cpp-type-modifiers+
                                                :test #'equal)
                                    (create-ast (nest (make-keyword)
                                                      (string-upcase str))
                                                str))
                                   ((member str +c/cpp-primitive-types+
                                                :test #'equal)
                                    (create-ast :primitive-type str))
                                   (:otherwise
                                    (create-ast :type-identifier str)))))
                   (remove-if «or {member _ +c/cpp-type-qualifiers+
                                          :test #'equal}
                                  {member _ +c/cpp-type-storage-classes+
                                          :test #'equal}»)
                   (mapcar #'strip-array-or-pointer split))))
    (or (gethash (cons type-string language) *type-from-trace-string-cache*)
        (setf (gethash (cons type-string language)
                       *type-from-trace-string-cache*)
              (make-instance 'c/cpp-canonical-type
                :declarator (append (pointer-declarator-component)
                                    (array-declarator-component))
                :specifier (append (type-qualifiers)
                                   (type-storage-classes)
                                   (base-type)))))))


;;; Variable instrumentation
(defparameter +identifier-regex+
  (create-scanner "^[A-Za-z_][A-Za-z0-9_]*$"))

(defmethod var-instrument ((key function)
                           (instrumenter c/cpp-instrumenter)
                           (ast c/cpp-ast)
                           &key (print-strings nil))
  "Generate ASTs for variable instrumentation.
* KEY a function used to pull the variable list out of AST
* INSTRUMENTER current instrumentation state
* AST the AST to instrument"
  (iter (for var in (funcall key ast))
        (and-let* ((software (software instrumenter))
                   (root (genome software))
                   (name (aget :name var))
                   (decl (aget :decl var))
                   (scope (aget :scope var))
                   ((var-instrument-p name decl scope root))
                   (type (canonicalize-type decl :software software)))
          (collect (cons name type) into names-and-types))
        (finally
         (return (instrument-c-exprs instrumenter
                                     names-and-types
                                     print-strings)))))

(-> var-instrument-p (string ast ast ast) (values boolean &optional))
(defun var-instrument-p (name decl scope root)
  "Return non-NIL if we should instrument this variable entry."
  (labels ((has-name-p (name)
             (not (emptyp name)))
           (attribute-p (name)
             (starts-with-subseq "__attribute" name))
           (valid-identifier-p (name)
             (scan +identifier-regex+ name))
           (variable-declaration-p (ast)
             (typep ast 'variable-declaration-ast))
           (enumerator-p (ast)
             (typep ast 'c/cpp-enumerator))
           (function-pointer-parameter-p (root scope decl)
             (let ((parents (nest (butlast)
                                  (take-until {eq scope})
                                  (get-parent-asts root decl))))
               (and (some (of-type 'c/cpp-function-declarator) parents)
                    (some (of-type 'c/cpp-parameter-list) parents)))))
    (and (has-name-p name)
         (valid-identifier-p name)
         (variable-declaration-p decl)
         (not (enumerator-p decl))
         (not (attribute-p name))
         (not (function-pointer-parameter-p root scope decl)))))

(defgeneric instrument-c-exprs (instrumenter names-and-types print-strings)
  (:documentation "Generate C code to print the values of expressions.
NAMES-AND-TYPES is a list of (string . clang-type) pairs.
Returns a list of strings containing C source code.")
  (:method ((instrumenter c/cpp-instrumenter)
            (names-and-types list)
            print-strings
            &aux (software (software instrumenter)))
    (labels
        ((base-typename (canonical-type)
           "Return the base typename of CANONICAL-TYPE,
            without additional specifiers."
           (nest (fmt "~{~a~^ ~}")
                 (mapcar #'source-text)
                 (remove-if «or (of-type 'c/cpp-storage-class-specifier)
                                (of-type 'c/cpp-type-qualifier)»)
                 (specifier canonical-type)))
         (array-type-p (type)
           (find :array (declarator type) :key #'car))
         (pointer-type-p (type)
           (find :pointer (declarator type) :key #'car))
         (c-string-type-p (type)
           "Is TYPE a C string type?"
           (let ((name (base-typename type)))
             (and (string= name "char")
                  (xor (array-type-p type) (pointer-type-p type)))))
         (cpp-string-type-p (type)
           "Is TYPE a C++ string type?"
           (let ((name (base-typename type)))
             (or (string= name "string")
                 (string= name "std::string"))))
         (string-type-p (type)
           "Is TYPE a string type?"
           (or (c-string-type-p type)
               (cpp-string-type-p type)))
         (type-format-and-size (type print-strings)
           (let ((name (base-typename type)))
             (cond
               ;; String
               ((and print-strings (string-type-p type))
                '(:__GT_TRACEDB_BLOB "0"))
               ;; Pointer
               ;; Use sizeof(void*) in case underlying type is not yet declared.
               ((or (pointer-type-p type) (array-type-p type))
                '(:__GT_TRACEDB_POINTER "sizeof(void*)"))
               ;; Signed integers
               ((member name
                        '("char" "int8_t" "wchar_t" "short int" "int16_t" "int"
                          "int32_t" "long int" "int64_t")
                        :test #'string=)
                (list :__GT_TRACEDB_SIGNED
                      (fmt "sizeof(~a)" name)))
               ;; Unsigned integers
               ((member name
                        '("unsigned char" "uint8_t" "unsigned short int" "uint16_t"
                          "unsigned int" "uint32_t" "unsigned long int" "uint64_t"
                          "size_t")
                        :test #'string=)
                (list :__GT_TRACEDB_UNSIGNED
                      (fmt "sizeof(~a)" name)))
               ((string= name "float")
                '(:__GT_TRACEDB_FLOAT "sizeof(float)"))
               ((string= name "double")
                '(:__GT_TRACEDB_FLOAT "sizeof(double)"))
               ;; Otherwise no instrumentation
               (t '(nil nil)))))
         (get-name-index (name)
           (or (gethash name (names instrumenter))
               (setf (gethash name (names instrumenter))
                     (hash-table-count (names instrumenter)))))
         (type-description-struct (c-type format size)
           (fmt "{~a, ~a, ~a}"
                (get-name-index c-type)
                format
                size))
         (get-type-index (type format size print-strings)
           (let* ((c-type (type-trace-string type))
                  (type-id (cons (and print-strings (string-type-p type))
                                 c-type)))
             (or (gethash type-id (types instrumenter))
                 (progn
                   ;; Adding new type: generate header string
                   (setf (gethash type-id (type-descriptions instrumenter))
                         (type-description-struct c-type format size))
                   (setf (gethash type-id (types instrumenter))
                         (hash-table-count (types instrumenter)))))))
         (make-instrumentation-function-call (function-name function-args)
           (when function-args
             (nest (create-instrumentation-ast (language-ast-class software))
                   (fmt "~a(~a, ~d, ~{~a~^, ~});"
                        function-name
                        +trace-instrument-log-variable-name+
                        (length function-args)
                        function-args)))))

      (iter (for (name . type) in names-and-types)
            (destructuring-bind (format size)
                (type-format-and-size type print-strings)
              (when format
                (let ((type-index (get-type-index type format size print-strings))
                      (name-index (get-name-index name)))
                  (cond
                    ;; C string
                    ((and print-strings (c-string-type-p type))
                     (collect (fmt "~d, ~d, strlen(~a), ~a"
                                   name-index type-index name name)
                       into blob-args))

                    ;; C++ string
                    ((and print-strings (cpp-string-type-p type))
                     (collect (fmt "~d, ~d, (~a).length(), (~a).c_str()"
                                   name-index type-index name name)
                       into blob-args))

                    ;; Normal variable
                    (t
                     (collect (fmt "~a, ~a, ~a, ~a, ~a"
                                   name-index type-index size format name)
                       into var-args))))))
            (finally
             (return
               (nest (remove-if #'null)
                     (append (list (make-instrumentation-function-call
                                    "__write_trace_variables"
                                    var-args))
                             (list (make-instrumentation-function-call
                                    "__write_trace_blobs"
                                    blob-args))))))))))


;;; Helper functions
(defgeneric write-trace-id (instrumenter ast)
  (:documentation "Generate ASTs which write statement ID to trace.
* INSTRUMENTER current instrumentation state
* AST the AST to instrument")
  (:method ((instrumenter c/cpp-instrumenter) (ast c/cpp-ast)
            &aux (sw (software instrumenter)))
    (nest (create-instrumentation-ast (language-ast-class sw))
          (fmt "~%__write_trace_id(~a, ~a, ~du);"
               +trace-instrument-log-variable-name+
               +instrument-log-lock-variable-name+
               (get-ast-id instrumenter ast)))))

(defgeneric write-end-entry (instrumenter)
  (:documentation "Generate ASTs which write end-entry flag to trace.
* INSTRUMENTER current instrumentation state")
  (:method ((instrumenter c/cpp-instrumenter)
            &aux (sw (software instrumenter)))
    (nest (create-instrumentation-ast (language-ast-class sw))
          (fmt "__write_end_entry(~a, ~a);~%"
               +trace-instrument-log-variable-name+
               +instrument-log-lock-variable-name+))))

(-> initialize-tracing (c/cpp-instrumenter
                        (or string pathname keyword null)
                        (or string null)
                        list)
                       (values c/cpp &optional))
(defun initialize-tracing (instrumenter file-name env-name entry-points
                           &aux (obj (software instrumenter)))
  "Insert code to initialize tracing and/or define the log variable.

* INSTRUMENTER current instrumentation state with software object to instrument
* FILE-NAME fixed name for the trace output file or stream (:stdout/:stderr)
* ENV-NAME environment variable from which to read the trace output file
* ENTRY-POINTS declarators for the main entry points, if present"
  (assert (typep obj 'c/cpp))

  (labels ((file-open-str ()
             ;; Open the file at FILE-NAME or in the environment variable
             ;; ENV-NAME.  If neither is given, read the file to open
             ;; from the buffer containing the contents of the
             ;; SEL_HANDSHAKE_FILE.
             (cond
               ((eq file-name :stderr) "stderr")
               ((eq file-name :stdout) "stdout")
               (file-name (fmt "fopen(~s, \"w\")" (namestring file-name)))
               ((stringp env-name) (fmt "fopen(getenv(~a), \"w\")" env-name))
               (t (fmt "fopen(buffer, \"w\")"))))
           (sort-names-alist (names-alist)
             (sort names-alist #'< :key #'cdr))
           (sort-types-alist (types-alist)
             (sort types-alist #'<
                   :key [{gethash _ (types instrumenter)} #'car]))
           (names-initialization-str ()
             (if (zerop (hash-table-count (names instrumenter)))
                 (fmt "const char **~a = NULL"
                      +names-variable-name+)
                 (fmt "const char *~a[] = {~{~s, ~}}"
                      +names-variable-name+
                      (nest (mapcar #'car)
                            (sort-names-alist)
                            (hash-table-alist)
                            (names instrumenter)))))
           (types-initialization-str ()
             (if (zerop (hash-table-count (types instrumenter)))
                 (fmt "const __trace_type_description *~a = NULL"
                      +types-variable-name+)
                 (fmt "const __trace_type_description ~a[] = {~{~a, ~}}"
                      +types-variable-name+
                      (nest (mapcar [#'source-text #'cdr])
                            (sort-types-alist)
                            (hash-table-alist)
                            (type-descriptions instrumenter))))))

    (when entry-points
      ;; Object contains main() so insert setup code. The goal is to
      ;; insert this exactly once in each executable while avoiding
      ;; link problems. It doesn't need to be in the same file as
      ;; main() but that provides a good heuristic.

      ;; The setup function uses a "constructor" attribute to run
      ;; before any other code. It optionally performs a handshake
      ;; with the trace collector, then opens the trace file and
      ;; writes the header.
      (mapcar {prepend-instrumentation-setup-code
               obj +write-trace-entry-macro+}
              entry-points)
      (nest (append-instrumentation-setup-code obj)
            (fmt +write-trace-initialization+
                 *trace-instrument-handshake-env-name*
                 (file-open-str)
                 (names-initialization-str)
                 (types-initialization-str)
                 (hash-table-count (names instrumenter))
                 (hash-table-count (types instrumenter))))))

  obj)

(defgeneric contains-main-p (obj)
  (:documentation "Return non-NIL if the main function exists in OBJ.")
  (:method ((obj c/cpp)) (not (null (main-declarators obj))))
  (:method ((obj t)) nil))

(defgeneric main-declarators (obj)
  (:documentation "Return the function declarators for main in OBJ, if possible.")
  (:method ((obj c/cpp))
    (nest (remove-if-not [{equal "main"} #'declarator-name])
          (remove-if-not (of-type 'c/cpp-function-declarator))
          (asts obj)))
  (:method ((obj t)) nil))

(-> create-instrumentation-ast (symbol string)
                               (values ast &optional))
(defun create-instrumentation-ast (ast-class text)
  "Create an AST of type AST-CLASS from TEXT with an instrumentation
annotation."
  (convert ast-class
           `((:class . :text-fragment)
             (:text . ,text)
             (:annotations . ((:instrumentation . t))))))

(-> instrumentation-ast-p (ast) (values t &optional))
(defun instrumentation-ast-p (ast)
  "Return non-NIL if AST is part of instrumentation."
  (aget :instrumentation (ast-annotations ast)))

(-> immediate-children (ast) (values list &optional))
(defun immediate-children (ast)
  "Return the immediate children (not below the first level) of AST."
  (remove-if-not [{length= 1} {ast-path ast}] (children ast)))

(-> concretize-error-ast (function ast) (values ast &optional))
(defun concretize-error-ast (getter ast)
  "If AST is an error node, replace it with a non-error child if possible."
  (if (member ast +error-ast-types+ :test #'typep)
      (or (nest (concretize-error-ast getter)
                (funcall getter)
                (immediate-children ast))
          ast)
      ast))

(-> prepend-instrumentation-setup-code (c/cpp string &optional ast)
                                       (values c/cpp &optional))
(defun prepend-instrumentation-setup-code (obj text &optional target)
  "Prepend TEXT with instrumentation setup to TARGET or
the top-level of OBJ."
  (nest (inject-instrumentation-setup-code obj text #'prepend-before-asts)
        (or target
            (nest (concretize-error-ast #'first)
                  (first)
                  (immediate-children)
                  (genome obj)))))

(-> append-instrumentation-setup-code (c/cpp string &optional ast)
                                      (values c/cpp &optional))
(defun append-instrumentation-setup-code (obj text &optional target)
  "Append TEXT with instrumentation setup to TARGET or
the top-level of OBJ."
  (nest (inject-instrumentation-setup-code obj text #'append-after-asts)
        (or target
            (nest (concretize-error-ast #'lastcar)
                  (lastcar)
                  (immediate-children)
                  (genome obj)))))

(-> inject-instrumentation-setup-code (c/cpp string function ast)
                                      (values c/cpp &optional))
(defun inject-instrumentation-setup-code (obj text action target)
  "Inject TEXT with instrumentation setup code at TARGET in OBJ."
  (nest (setf (genome obj))
        (with (genome obj) target)
        (funcall action target)
        (list)
        (create-instrumentation-ast (language-ast-class obj) text))
  obj)

(-> wrap-before-after-asts (ast list list) (values ast &optional))
(defun wrap-before-after-asts (ast before-asts after-asts)
  "Prepend BEFORE-ASTS and append AFTER-ASTS to AST."
  (prepend-before-asts (append-after-asts ast after-asts) before-asts))

(-> prepend-before-asts (ast list) (values ast &optional))
(defun prepend-before-asts (ast before-asts)
  "Prepend BEFORE-ASTS to AST's before-asts slot."
  (copy ast :before-asts (append (before-asts ast) before-asts)))

(-> append-after-asts (ast list) (values ast &optional))
(defun append-after-asts (ast after-asts)
  "Append AFTER-ASTS to AST's after-asts slot."
  (copy ast :after-asts (append (after-asts ast) after-asts)))
