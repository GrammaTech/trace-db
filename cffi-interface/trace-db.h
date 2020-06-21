//
// trace-db.h -- C interface for the trace database.
//

#ifndef __TRACE_DB_H
#define __TRACE_DB_H

#include <cstdint>

extern "C" {

/*
   Allocate a trace DB.
*/
void /* TraceDB */ *create_db();

/*
   Read a trace into the DB returning 1 on success and 0 on failure.
*/
int add_trace(void /* TraceDB */ *db_ptr,
              const char *filename,
              uint32_t timeout_seconds,
              uint64_t max_points);

/*
   Add in-memory trace points to the DB,
   returning 1 on success and 0 on failure.

   IMPORTANT: This method is for legacy testing purposes only.
   This method is not complete or efficient.
   DO NOT USE IN PRODUCTION OR NEW DEVELOPMENT.
*/
int add_trace_points(void /* TraceDB */ *db_ptr,
                     c_trace_point_struct* points,
                     uint64_t n_points,
                     char** c_names,
                     uint32_t n_names);

/*
   Return a pointer to the trace in DB_PTR at INDEX.
*/
const void /* Trace */ *get_trace(const void /* TraceDB */ *db_ptr,
                                  uint32_t index);

/*
   Return the number of traces in the trace database.
*/
uint32_t n_traces(const void /* TraceDB */ *db_ptr);

/*
   Serialize a trace DB a string.
*/
char* serialize_trace_db(const void /* TraceDB */ *db_ptr);

/*
   Deserialize a trace DB from a string.
*/
void* deserialize_trace_db(const char* text);

/*
   Deallocate a trace DB.
*/
void free_db(void /* TraceDB */ *db_ptr);

/*
   QUERY PREDICATES

   A predicate is a simple expression tree containing logical and arithmetic
   operators, numeric comparisons, variables, and numeric values.

   Predicates are type-checked during query processing, and will generate
   assertion failures if, for example, a number is used where a boolean value
   is required.

   This is an analog to the Predicate class in QueryObjects.hpp
   required for interoperability with the LISP CFFI interface.
*/
typedef struct c_predicate
{
    enum predicate_kind kind;
    uint64_t n_children;
    uint64_t var_index;
    // CFFI does not handle union types properly
    // so we have separate variables for each
    // type and use the appropriate value based
    // on the predicate kind.
    uint64_t unsigned_value;
    int64_t signed_value;
    double float_value;
    struct c_predicate *children;
} c_predicate;

/*
   Deallocate a query predicate.
*/
void free_c_predicate(c_predicate *c_predicate);

/*
   FREE VARIABLES

   A free variable is a placeholder for a variable binding in a trace point.
   The struct defines the allowed types for the variable binding.

   This is an analog to the FreeVariable class in QueryObjects.hpp
   required for interoperability with the LISP CFFI interface.
*/
typedef struct c_free_variable
{
    uint32_t n_allowed_types;
    /* Indices of compatible types */
    uint32_t *allowed_types;
} c_free_variable;

/*
   Return all results which satisify the predicate.

   Each result is represented as a trace point containing only the statement
   ID and bound variables.

   *n_results_out will be set to the number of results. If the number of
   *results is non-zero, then *results_out will be set to an array of
   *results.

   The caller must pass the results to free_query_result to deallocate
   memory.

   Results are generated at each trace point by finding all possible bindings
   for the free variables, then evaluating the predicate over those variables
   to filter. If there are no free variables, each trace point will generate
   one result.  If soft predicates are given, we perform an additional round
   of filtering to return those results which maximize the number of soft
   predicates satisfied.

   If seed is non-zero, randomly choose a single trace point and return results
   from that point.

   If keep_duplicate_bindings is false (0), remove trace points with
   duplicate variable bindings.

   Trace points where (statement_id & statement_mask) != statement are
   skipped. Setting both arguments to zero will ensure that all statements
   are queried.
*/
void query_trace(const void /* TraceDB */ *db_ptr, uint64_t index,
                 const c_free_variable *variables, uint32_t n_variables,
                 const c_predicate *hard_predicate,
                 const c_predicate **soft_predicates, uint32_t n_soft_predicates,
                 uint32_t seed, uint8_t keep_duplicate_bindings,
                 uint64_t statement_mask, uint64_t statement,
                 c_trace_point_struct **results_out, uint64_t *n_results_out);

} // end extern "C"

#endif // __TRACE_DB_H
