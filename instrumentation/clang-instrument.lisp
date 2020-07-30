;;; clang-instrument.lisp --- Instrument C-language source files.
(defpackage :trace-db/instrumentation/clang-instrument
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/utility/task
        :software-evolution-library/components/fodder-database
        :software-evolution-library/software/parseable
        :software-evolution-library/software/compilable
        :software-evolution-library/software/clang
        :software-evolution-library/software/project
        :software-evolution-library/software/clang-project
        :trace-db/binary-trace-db
        :trace-db/instrumentation/instrument)
  (:import-from :functional-trees :path-later-p)
  (:export :clang-instrumenter
           :instrument-c-exprs))
(in-package :trace-db/instrumentation/clang-instrument)
(in-readtable :curry-compose-reader-macros)


;;;; Instrumentation
(define-constant +instrument-log-lock-variable-name+ "__sel_trace_file_lock"
  :test #'string=
  :documentation "File lock variable used for instrumentation")

(define-constant +write-trace-forward-declarations+
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
extern void * " +instrument-log-variable-name+ ";
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
static void __write_trace_aux(void *out, const unsigned long long value);

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
  :test #'string=
  :documentation "C code to include in all instrumented files.")


(define-constant +write-trace-implementation+
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

static void __write_trace_aux(void *out, const unsigned long long value)
{
    fputc(__GT_TRACEDB_AUXILIARY, (FILE *) out);
    fwrite(&value, sizeof(uint64_t), 1, (FILE *) out);
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
  :test #'string=
  :documentation "C code which implements trace writing.")

(define-constant +names-variable-name+ "names"
  :test #'string=
  :documentation "Name of the variable containing instrumentation var names.")

(define-constant +types-variable-name+ "types"
  :test #'string=
  :documentation "Name of the variable containing instrumentation types.")

(define-constant +write-trace-initialization+
    (concatenate 'string "
#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

void * " +instrument-log-variable-name+ ";
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

  " +instrument-log-variable-name+ " = ~a;
  " +instrument-log-lock-variable-name+ " = &__static_lock;
  ~a;
  ~a;

  pthread_mutex_init( (pthread_mutex_t *) " +instrument-log-lock-variable-name+
  ", NULL);
  __write_trace_header(" +instrument-log-variable-name+ ",
                       " +instrument-log-lock-variable-name+ ",
                       " +names-variable-name+ ", ~d,
                       " +types-variable-name+ ", ~d);
}
")
  :test #'string=
  :documentation "C code which initializes the trace file")

(defclass clang-instrumenter (instrumenter)
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
  (:documentation "Handles instrumentation for clang software objects."))


(defun array-or-pointer-type (type)
  "Is TYPE an array or pointer (but not an array of pointers)?
* TYPE a type object
"
  ;; array or pointer, but not array of pointers
  (xor (not (emptyp (type-array type)))
       (type-pointer type)))

(defun get-ast-id (instrumenter ast)
  "Return the trace ID for AST
* INSTRUMENTER current instrumentation state
* AST the AST to instrument
"
  (gethash ast (ast-ids instrumenter)))

(defun get-ast-ids-ht (obj &optional file-id)
  "Return a hash table mapping AST index to
trace statement ID for each AST in OBJ.
* OBJ CLANG software object
* FILE-ID index of OBJ in a project
"
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


(defgeneric write-trace-id (instrumenter ast)
  (:documentation "Generate ASTs which write statement ID to trace.
* INSTRUMENTER current instrumentation state
* AST the AST to instrument
"))

(defmethod write-trace-id ((instrumenter clang-instrumenter) ast)
  "Generate ASTs which write statement ID to trace.
* INSTRUMENTER current instrumentation state
* AST the AST to instrument
"
  (make-call-expr "__write_trace_id"
                  (list (make-var-reference +instrument-log-variable-name+
                                            nil)
                        (make-var-reference +instrument-log-lock-variable-name+
                                            nil)
                        (make-literal (get-ast-id instrumenter ast) :unsigned))
                  :fullstmt
                  :full-stmt t
                  :annotations '((:instrumentation t))))

(defgeneric write-trace-aux (instrumenter value)
  (:documentation "Generate ASTs which write aux entries to trace.
* INSTRUMENTER current instrumentation state
* VALUE the auxiliary value to write
"))

(defmethod write-trace-aux ((instrumenter clang-instrumenter) value)
  "Generate ASTs which write aux entries to trace.
* INSTRUMENTER current instrumentation state
* VALUE the auxiliary value to write
"
  (declare (ignorable instrumenter))
  (make-call-expr "__write_trace_aux"
                  (list (make-var-reference +instrument-log-variable-name+ nil)
                        (make-literal value))
                  :fullstmt
                  :full-stmt t
                  :annotations '((:instrumentation t))))

(defgeneric write-end-entry (instrumenter)
  (:documentation "Generate ASTs which write end-entry flag to trace.
* INSTRUMENTER current instrumentation state
"))

(defmethod write-end-entry ((instrumenter clang-instrumenter))
  "Generate ASTs which write end-entry flag to trace.
* INSTRUMENTER current instrumentation state
"
  (declare (ignorable instrumenter))
  (make-call-expr "__write_end_entry"
                  (list (make-var-reference +instrument-log-variable-name+
                                            nil)
                        (make-var-reference +instrument-log-lock-variable-name+
                                            nil))
                  :fullstmt
                  :full-stmt t
                  :annotations '((:instrumentation t))))

(defgeneric instrument-return (instrumenter return-stmt return-void)
  (:documentation "Generate ASTs which instrument RETURN-STMT.
The generated ASTs will store the return value in a temporary variable
and jump to the exit point, allowing instrumentation of function exit.
* INSTRUMENTER current instrumentation state
* RETURN-STMT the AST for a return statement
* RETURN-VOID does this function return void?
"))

(defmethod instrument-return ((instrumenter clang-instrumenter)
                              return-stmt return-void)
  "Generate ASTs which instrument RETURN-STMT.
The generated ASTs will store the return value in a local variable
and jump to the exit point, allowing instrumentation of function exit.
* INSTRUMENTER current instrumentation state
* RETURN-STMT the AST for a return statement
* RETURN-VOID does this function return void?
"
  (if return-void
      `(,(make-statement :gotostmt :fullstmt '("goto inst_exit")
                         :full-stmt t
                         :annotations '((:instrumentation t))))
      `(,(make-operator :fullstmt "="
                        (list (make-var-reference "_inst_ret" nil)
                              (car (child-asts return-stmt)))
                        :full-stmt t
                        :annotations '((:instrumentation t)))
        ,(make-statement :gotostmt :fullstmt '("goto inst_exit")
                         :full-stmt t
                         :annotations '((:instrumentation t))))))

(defgeneric instrument-exit (instrumenter function return-void)
  (:documentation "Generate ASTs to instrument exit from FUNCTION.

The generated ASTs add a label which can be jumped to from return
statements, and return a value stored in a local variable, allowing
instrumentation of function exit.

* INSTRUMENTER current instrumentation state
* FUNCTION the AST for the current function
* RETURN-VOID does this function return void?
"))

(defmethod instrument-exit ((instrumenter clang-instrumenter)
                            function return-void)
  "Generate ASTs to instrument exit from FUNCTION.

The generated ASTs add a label which can be jumped to from return
statements, and return a value stored in a local variable, allowing
instrumentation of function exit.

* INSTRUMENTER current instrumentation state
* FUNCTION the AST for the current function
* RETURN-VOID does this function return void?
"
  (let ((obj (software instrumenter)))
    `(,(make-label "inst_exit"
                   ;; ast-id hash table uses eq, but function-body will
                   ;; generate a new ast. Search for the original in
                   ;; order to look up the id.
                   (write-trace-id instrumenter
                                   (find-if {equalp (function-body function)}
                                            (asts obj)))
                   :annotations '((:instrumentation t)))
      ,(write-end-entry instrumenter)
      ,(make-statement :ReturnStmt :fullstmt
                       (cons "return "
                             (when (not return-void)
                               (list (make-var-reference "_inst_ret"
                                                         nil))))
                       :full-stmt t
                       :annotations '((:instrumentation t))))))

(defmethod instrumented-p ((clang clang))
  "Return true if CLANG is instrumented
* CLANG a clang software object
"
  (search +instrument-log-variable-name+ (genome-string clang)))

(defmethod instrument ((obj clang) &rest args)
  "Instrumentation for clang software objects.
Creates a CLANG-INSTRUMENTER for OBJ and calls its instrument method.

* OBJ the software object to instrument
* ARGS additional arguments are passed through to the instrumenter method.
"
  (apply #'instrument
         (make-instance 'clang-instrumenter
           :software obj
           :ast-ids (get-ast-ids-ht obj))
         args))



(defun instrument-create-value (obj ast return-type before after
                                instrumenter instrument-exit)
  (let* (;; Look up AST again in case its children have been
         ;; instrumented
         (wrap (not (traceable-stmt-p obj ast)))
         (new-ast (@ obj (ast-path obj ast)))
         (stmts (append (mapcar {add-semicolon _ :after} before)
                        (if (and instrument-exit
                                 (eq (ast-class ast) :ReturnStmt))
                            (nest (mapcar {add-semicolon _ :both})
                                  (instrument-return instrumenter
                                                     new-ast
                                                     (null return-type)))
                            (list new-ast))
                        (mapcar {add-semicolon _ :both} after))))
    ;; Wrap in compound statement if needed
    (if wrap
        (make-statement
         :CompoundStmt
         :FullStmt
         `("{" ,@(interleave stmts ";") ";}")
         :full-stmt t
         :annotations '((:instrumentation t)))
        stmts)))

(defgeneric enclosing-traceable-stmt (software ast)
  (:documentation "Return the first ancestor of AST in SOFTWARE which may
be a full stmt.  If a statement is reached which is not itself traceable,
but which could be made traceable by wrapping with curly braces, return that.")
  (:method ((obj clang) (ast clang-ast))
    (cond
      ((traceable-stmt-p obj ast) ast)
      ;; Wrap AST in a CompoundStmt to make it traceable.
      ((can-be-made-traceable-p obj ast) ast)
      (:otherwise
       (when-let ((parent (get-parent-ast obj ast)))
         (enclosing-traceable-stmt obj parent))))))

(defmethod can-be-made-traceable-p ((obj clang) (ast clang-ast))
  (or (traceable-stmt-p obj ast)
      (unless (or (ast-guard-stmt ast) ; Don't wrap guard statements.
                  (eq :CompoundStmt ; Don't wrap CompoundStmts.
                      (ast-class ast)))
        (when-let ((parent (get-parent-ast obj ast)))
          ;; Is a child of a statement which might have a hanging body.
          (member (ast-class parent) +clang-wrapable-parents+)))))

(defmethod traceable-stmt-p ((obj clang) (ast clang-ast))
  (and (ast-full-stmt ast)
       (not (function-decl-p ast))
       (not (ast-in-macro-expansion ast))
       (not (eq :NullStmt (ast-class ast)))
       (get-parent-ast obj ast)
       (eq :CompoundStmt (ast-class (get-parent-ast obj ast)))))

(defun last-traceable-stmt (obj proto)
  "The last traceable statement in the body of a function
declaration, or NIL if PROTO is not a function declaration or there
is no such traceable statement."
  (nest (enclosing-traceable-stmt obj)
        (lastcar)
        (child-asts)
        (function-body proto)))

(defun first-traceable-stmt (proto)
  "The first traceable statement in the body of a function
declaration, or NIL if PROTO is not a function declaration or there
is no such traceable statement."
  (first (child-asts (function-body proto))))

(defmethod instrument
    ((instrumenter clang-instrumenter)
     &key points functions functions-after trace-file trace-env instrument-exit
       (filter (constantly t)) (num-threads 0))
  "Use INSTRUMENTER to instrument a clang software object.

* INSTRUMENTER current instrumentation state
* POINTS alist of additional values to print at specific points
* FUNCTIONS  functions to calculate instrumentation at each point
* FUNCTIONS-AFTER functions to calculate instrumentation after each point
* TRACE-FILE file or stream (stdout/stderr) for trace output
* TRACE-ENV trace output to file specified by ENV variable
* INSTRUMENT-EXIT print counter of function body before exit
* FILTER function to select a subset of ASTs for instrumentation
         function should take a software object and an AST parameters,
         returning nil if the AST should be filtered from instrumentation
* NUM-THREADS number of threads to use for instrumentation"
  (declare (ignorable num-threads))
  (let* ((obj (software instrumenter))
         (entry (get-entry obj))
         ;; Promote every counter key in POINTS to the enclosing full
         ;; statement with a CompoundStmt as a parent.  Otherwise they
         ;; will not appear in the output.
         (points
          (remove nil
            (mapcar (lambda (pair)
                      (destructuring-bind (ast . value) pair
                        (let ((parent (enclosing-traceable-stmt obj ast)))
                          (if parent (cons parent value)
                              (warn "Point ~s doesn't match traceable AST."
                                    ast)))))
                    points))))
    (labels
        ((sort-asts (obj asts)
           (sort asts #'path-later-p :key {ast-path obj}))
         (instrument-asts (obj)
           "Generate instrumentation for all ASTs in OBJ.  As a side-effect,
update POINTS after instrumenting ASTs."
           (nest (mapcar
                   (lambda (ast)
                     (prog1
                         (instrument-ast ast
                                         (mappend {funcall _ instrumenter ast}
                                                  functions)
                                         (mappend {funcall _ instrumenter ast}
                                                  functions-after)
                                         (when points
                                           (aget ast points :test #'equalp)))
                       (when points
                         (setf (aget ast points :test #'equalp) nil)))))
                 (sort-asts obj)
                 (remove-if-not {funcall filter obj})
                 (remove-if-not {can-be-made-traceable-p obj})
                 (asts obj)))
         (instrument-ast (ast extra-stmts extra-stmts-after aux-values)
           "Generate instrumentation for AST.
Returns a list of (AST RETURN-TYPE INSTRUMENTATION-BEFORE INSTRUMENTATION-AFTER).
"

           (let* ((function (when instrument-exit
                              (function-containing-ast obj ast)))
                  (return-type (when (and function
                                          (not (ast-void-ret function)))
                                 (find-type obj (ast-ret function)))))

             `(,ast
               ,return-type
               ;; Instrumentation before
               (;; Temp variable for return value
                ,@(when (and instrument-exit
                             (equalp ast (first-traceable-stmt function))
                             return-type)
                        `(,(make-var-decl "_inst_ret" return-type nil
                                          :annotations '((:instrumentation t)))))
                ;; Instrument before
                ,(write-trace-id instrumenter ast)
                ,@(mapcar {write-trace-aux instrumenter} aux-values)
                ,@extra-stmts
                ,(write-end-entry instrumenter))
               (;; Instrumentation after
                ,@(when functions-after
                        `(,(write-trace-id instrumenter ast)
                          ,@extra-stmts-after
                          ,(write-end-entry instrumenter)))
                ;; Function exit instrumentation
                ,@(when (and instrument-exit
                             (equalp ast (last-traceable-stmt obj function)))
                        (instrument-exit instrumenter function
                                         (null return-type))))))))

    ;; Apply mutations to instrument OBJ.
    ;; Note: These mutations are sent as a list to `apply-mutation-ops`
    ;; and are performed sequently.  This offers a performance improvement
    ;; over calling `apply-mutation-ops` multiple times.
    (apply-mutation-ops
      obj
      (iter (for (ast return-type before after) in (instrument-asts obj))
            (collect (if (not (traceable-stmt-p obj ast))
                         `(:set (:stmt1 . ,ast)
                                (:value1 . ,{instrument-create-value
                                             obj
                                             ast
                                             return-type
                                             before
                                             after
                                             instrumenter
                                             instrument-exit}))
                         `(:splice (:stmt1 . ,ast)
                                   (:value1 . ,{instrument-create-value
                                                obj
                                                ast
                                                return-type
                                                before
                                                after
                                                instrumenter
                                                instrument-exit})))))))

    ;; Warn about any leftover un-inserted points.
    (mapc (lambda (point)
            (warn "No insertion point found for point ~a." point))
          (remove-if-not #'cdr points))

    ;; Add support code for tracing to obj
    (prepend-text-to-genome obj +write-trace-forward-declarations+)
    (append-text-to-genome obj +write-trace-implementation+)
    (initialize-tracing instrumenter trace-file trace-env entry)

    ;; Add flag to allow building with pthreads
    (appendf (flags obj) (list "-lpthread"))

    obj))

(defmethod uninstrument ((clang clang) &key (num-threads 0))
  "Remove instrumentation from CLANG"
  (declare (ignorable num-threads))
  (labels ((uninstrument-genome-prologue (clang)
             (with-slots (genome) clang
               (setf genome
                 (copy genome
                       :children
                       (cons (replace-all (first (children genome))
                                          +write-trace-forward-declarations+
                                          "")
                             (cdr (children genome)))))))
           (uninstrument-genome-epilogue (clang)
             (with-slots (genome) clang
               (setf genome
                 (copy genome
                       :children
                       (let ((last-child (nest (lastcar)
                                               (children)
                                               (genome clang))))
                         (if (stringp last-child)
                             (append
                              (butlast (children (genome clang)))
                              (list (subseq last-child
                                            0
                                            (search +write-trace-implementation+
                                                    last-child))))
                             (children (genome clang)))))))))

    ;; Remove instrumentation setup code
    (uninstrument-genome-prologue clang)
    (uninstrument-genome-epilogue clang)

    ;; Remove instrumented ASTs - blocks first, then individual statements
    (let ((*matching-free-var-retains-name-bias* 1.0)
          (*matching-free-function-retains-name-bias* 1.0))
      (apply-mutation-ops
        clang
        (iter (for ast in (nest (reverse)
                                (remove-if-not #'block-p)
                                (remove-if-not [{aget :instrumentation}
                                                #'ast-annotations])
                                (asts clang)))
              (collect `(:set (:stmt1 . ,ast)
                              (:value1 .
                               ,{lookup clang
                                        (nest (ast-path clang)
                                              (first)
                                              (remove-if
                                                [{aget :instrumentation}
                                                 #'ast-annotations])
                                              (child-asts)
                                              (lookup clang)
                                              (ast-path clang ast))})))))

      (apply-mutation-ops
        clang
        (iter (for ast in (nest (reverse)
                                (remove-if #'block-p)
                                (remove-if-not [{aget :instrumentation}
                                                #'ast-annotations])
                                (asts clang)))
              (collect `(:splice (:stmt1 . ,ast) (:value1 . nil)))))))

  ;; Return the software object
  clang)

(defmethod instrumentation-files ((clang-project clang-project))
  "Return files in CLANG-PROJECT in the order which they would be instrumented
* CLANG-PROJECT an instrumented project
"
  (append (remove-if {get-entry}
                     (evolve-files clang-project)
                     :key #'cdr)
          (remove-if [#'not {get-entry}]
                     (evolve-files clang-project)
                     :key #'cdr)))

(defmethod instrument ((clang-project clang-project) &rest args
    &aux (names (make-thread-safe-hash-table :test #'equalp))
         (types (make-thread-safe-hash-table :test #'equalp))
         (type-descriptions (make-thread-safe-hash-table :test #'equalp))
         (files (instrumentation-files clang-project))
         (num-threads (or (plist-get :num-threads args) 0)))
  "Instrument CLANG-PROJECT to print AST index before each full statement.

* CLANG-PROJECT the project to instrument
* ARGS passed through to the instrument method on underlying software objects."
  ;; Instrument the non-entry point files in the project in parallel.
  (task-map num-threads
            (lambda (instrumenter)
              (apply #'instrument instrumenter args))
            (iter (for obj in (remove-if #'get-entry (mapcar #'cdr files)))
                  (for file-id upfrom 0)
                  (collect (make-instance 'clang-instrumenter
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
        (when (get-entry obj)
          (apply #'instrument
                 (make-instance 'clang-instrumenter
                   :software obj
                   :names names
                   :types types
                   :type-descriptions type-descriptions
                   :ast-ids (get-ast-ids-ht obj file-id))
                 args)))

  ;; Insert log setup code in other-files with an entry point.
  (iter (for obj in (mapcar #'cdr (other-files clang-project)))
        (when-let ((entry (get-entry obj)))
          (prepend-text-to-genome obj +write-trace-forward-declarations+)
          (append-text-to-genome obj +write-trace-implementation+)
          (initialize-tracing (make-instance 'clang-instrumenter
                                :software obj
                                :names names
                                :types types
                                :type-descriptions type-descriptions)
                              (plist-get :trace-file args)
                              (plist-get :trace-env args)
                              entry)))

  clang-project)

(defgeneric instrument-c-exprs (instrumenter exprs-and-types print-strings)
  (:documentation "Generate C code to print the values of expressions.
EXPRS-AND-TYPES is a list of (string . clang-type) pairs.
Returns a list of strings containing C source code."))

(defmethod instrument-c-exprs ((instrumenter clang-instrumenter)
                               exprs-and-types print-strings)
  (labels
      ((string-type-p (type)
         (or (string= (type-name type) "string")
             (string= (type-name type) "std::string")
             (and (string= (type-name type) "char")
                  (array-or-pointer-type type))))

       (type-format-and-size (type print-strings)
         (let ((unqualified-c-type (type-trace-string type :qualified nil)))
           (cond
             ;; String
             ((and print-strings (string-type-p type))
              '(:__GT_TRACEDB_BLOB "0"))
             ;; Pointer
             ;; Use sizeof(void*) in case underlying type is not yet declared.
             ((or (starts-with "*" unqualified-c-type :test #'string=)
                  (starts-with "[" unqualified-c-type :test #'string=))
              '(:__GT_TRACEDB_POINTER "sizeof(void*)"))
             ;; Signed integers
             ((member unqualified-c-type
                      '("char" "int8_t" "wchar_t" "short" "int16_t" "int"
                        "int32_t" "long" "int64_t")
                      :test #'name=)
              (list :__GT_TRACEDB_SIGNED
                    (format nil "sizeof(~a)" (type-qual (ct+-type type)))))
             ;; Unsigned integers
             ((member unqualified-c-type
                      '("unsigned char" "uint8_t" "unsigned short" "uint16_t"
                        "unsigned int" "uint32_t" "unsigned long" "uint64_t"
                        "size_t")
                      :test #'name=)
              (list :__GT_TRACEDB_UNSIGNED
                    (format nil "sizeof(~a)" (type-qual (ct+-type type)))))
             ((name= unqualified-c-type "float")
              '(:__GT_TRACEDB_FLOAT "sizeof(float)"))
             ((name= unqualified-c-type "double")
              '(:__GT_TRACEDB_FLOAT "sizeof(double)"))
             ;; Otherwise no instrumentation
             (t '(nil nil)))))

       (get-name-index (name)
         (or (gethash name (names instrumenter))
             (setf (gethash name (names instrumenter))
                   (hash-table-count (names instrumenter)))))

       (type-description-struct (c-type format size)
         (format nil "{~a, ~a, ~a}"
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
       (make-instrumentation-ast (function-name function-args)
         (when function-args
           (make-statement :CallExpr :FullStmt
                           `(,(make-statement :ImplictCastExpr :generic
                                (list (make-statement :DeclRefExpr :generic
                                                      (list function-name))))
                             ,(format nil "(~a, ~d, ~{~a~^, ~})"
                                          +instrument-log-variable-name+
                                          (length function-args)
                                          function-args))
                           :full-stmt t
                           :annotations '((:instrumentation t))))))

    (iter (for (expr . type) in exprs-and-types)
          (destructuring-bind (format size)
              (type-format-and-size type print-strings)
            (when format
              (let ((type-index (get-type-index type format size print-strings))
                    (name-index (get-name-index expr)))
                (cond
                  ;; C string
                  ((and print-strings
                        (array-or-pointer-type type)
                        (string= "char" (type-name type)))
                   (collect (format nil "~d, ~d, strlen(~a), ~a"
                                    name-index type-index expr expr)
                     into blob-args))

                  ;; C++ string
                  ((and print-strings
                        (or (string= "string" (type-name type))
                            (string= "std::string" (type-name type))))
                   (collect (format nil
                                    "~d, ~d, (~a).length(), (~a).c_str()"
                                    name-index type-index expr expr)
                     into blob-args))

                  ;; Normal variable
                  (t
                   (collect
                       (format nil "~a, ~a, ~a, ~a, ~a"
                               name-index type-index size format (ast-name expr))
                     into var-args))))))
          (finally
           (return
             (nest (remove-if #'null)
                   (append (list (make-instrumentation-ast
                                  "__write_trace_variables"
                                  var-args))
                           (list (make-instrumentation-ast
                                  "__write_trace_blobs"
                                  blob-args)))))))))

(defmethod var-instrument
    (key (instrumenter clang-instrumenter) (ast clang-ast) &key print-strings)
  "Generate ASTs for variable instrumentation.
* KEY a function used to pull the variable list out of AST
* INSTRUMENTER current instrumentation state
* AST the AST to instrument
"
  (iter (for var in (funcall key ast))
        (when-let* ((software (software instrumenter))
                    (type (when-let ((var-type (find-var-type software var)))
                            (typedef-type software var-type)))
                    (name (aget :name var))
                    ;; Don't instrument nameless variables
                    (has-name (not (emptyp (source-text name)))))
          (collect (cons (ast-name name) type) into names-and-types))
        (finally
         (return (instrument-c-exprs instrumenter names-and-types
                                     print-strings)))))

(defun initialize-tracing (instrumenter file-name env-name contains-entry
                           &aux (obj (software instrumenter)))
  "Insert code to initialize tracing and/or define the log variable.

* INSTRUMENTER current instrumentation state with software object to instrument
* FILE-NAME fixed name for the trace output file or stream (:stdout/:stderr)
* ENV-NAME environment variable from which to read the trace output file
* CONTAINS-ENTRY does this object contain the entry point?
"
  (assert (typep obj 'clang))

  (labels ((file-open-str ()
             ;; Open the file at FILE-NAME or in the environment variable
             ;; ENV-NAME.  If neither is given, read the file to open
             ;; from the buffer containing the contents of the
             ;; SEL_HANDSHAKE_FILE.
             (cond
               ((eq file-name :stderr) "stderr")
               ((eq file-name :stdout) "stdout")
               (file-name (format nil "fopen(~s, \"w\")" (namestring file-name)))
               ((stringp env-name) (format nil "fopen(getenv(~a), \"w\")" env-name))
               (t (format nil "fopen(buffer, \"w\")"))))
           (sort-names-alist (names-alist)
             (sort names-alist #'< :key #'cdr))
           (sort-types-alist (types-alist)
             (sort types-alist #'<
                   :key [{gethash _ (types instrumenter)} #'car]))
           (names-initialization-str ()
             (if (zerop (hash-table-count (names instrumenter)))
                 (format nil "const char **~a = NULL"
                         +names-variable-name+)
                 (format nil "const char *~a[] = {~{~s, ~}}"
                         +names-variable-name+
                         (nest (mapcar [#'ast-name #'car])
                               (sort-names-alist)
                               (hash-table-alist)
                               (names instrumenter)))))
           (types-initialization-str ()
             (if (zerop (hash-table-count (types instrumenter)))
                 (format nil "const __trace_type_description *~a = NULL"
                         +types-variable-name+)
                 (format nil "const __trace_type_description ~a[] = {~{~a, ~}}"
                         +types-variable-name+
                         (nest (mapcar [#'source-text #'cdr])
                               (sort-types-alist)
                               (hash-table-alist)
                               (type-descriptions instrumenter))))))

    (when contains-entry
      ;; Object contains main() so insert setup code. The goal is to
      ;; insert this exactly once in each executable while avoiding
      ;; link problems. It doesn't need to be in the same file as
      ;; main() but that provides a good heuristic.

      ;; The setup function uses a "constructor" attribute to run
      ;; before any other code. It optionally performs a handshake
      ;; with the trace collector, then opens the trace file and
      ;; writes the header.
      (append-text-to-genome obj
                             (format nil +write-trace-initialization+
                                     *instrument-handshake-env-name*
                                     (file-open-str)
                                     (names-initialization-str)
                                     (types-initialization-str)
                                     (hash-table-count (names instrumenter))
                                     (hash-table-count (types instrumenter))))))

  obj)