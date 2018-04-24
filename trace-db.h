#ifndef __TRACE_DB_H
#define __TRACE_DB_H

#ifdef __cplusplus
extern "C" {
#endif
#include "read-trace.h"

typedef struct trace
{
    trace_point *points;
    uint64_t n_points;
    uint64_t n_points_allocated;

    const char **names;
    uint32_t n_names;
    const type_description *types;
    uint32_t n_types;
} trace;

typedef struct trace_db
{
    trace *traces;
    uint64_t n_traces;
    uint64_t n_traces_allocated;
} trace_db;

trace_db *create_db();
/* Read a trace into the DB */
void add_trace(trace_db *db, trace_read_state *state, uint64_t max);
/* Store an existing trace in the DB */
void set_trace(trace_db *db, uint32_t index, trace *trace);
void free_db(trace_db *db);

typedef struct free_variable
{
    uint32_t n_allowed_types;
    /* Indices of compatible types */
    uint32_t *allowed_types;
} free_variable;

/* QUERY PREDICATES

   A predicate is a simple expression tree containing logical and arithmetic
   operators, numeric comparisons, variables, and numeric values.

   Predicates are type-checked during query processing, and will generate
   assertion failures if, for example, a number is used where a boolean value
   is required.
*/
enum predicate_kind
{
    /* Properties of variables */
    VAR_REFERENCE,
    VAR_SIZE,
    VAR_VALUE,
    DISTINCT_VARS,
    /* Numeric values */
    SIGNED_INTEGER,
    UNSIGNED_INTEGER,
    /* Logical operators */
    AND,
    OR,
    NOT,
    /* Comparisons */
    GREATER_THAN,
    LESS_THAN,
    EQUAL,
    /* Arithmetic operators */
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE
};

typedef struct predicate
{
    enum predicate_kind kind;
    union {
        uint64_t n_children;
        uint64_t var_index;
        uint64_t unsigned_value;
        int64_t signed_value;
    } data;
    struct predicate *children;
} predicate;

/* Return all results which match query.

   Each result is represented as a trace point containing only the statement
   ID and bound variables.

   If pick is true, randomly choose a single trace point and return results
   from that point.

   *n_results_out will be set to the number of results. If the number of
   *results is non-zero, then *results_out will be set to an array of
   *results.

   The caller must pass the results to free_query_result to deallocate
   memory.

   Results are generated at each trace point by finding all possible bindings
   for the free variables, then evaluating the predicate over those variables
   to filter. If there are no free variables, each trace point will generate
   one result.

   Trace points where (statement_id & statement_mask) != statement are
   skipped. Setting both arguments to zero will ensure that all statements
   are queried.
*/
void query_trace(const trace_db *db, uint64_t index,
                 uint32_t n_variables, const free_variable *variables,
                 const predicate *hard_predicate,
                 const predicate **soft_predicates, uint32_t n_soft_predicates,
                 uint32_t seed, uint64_t statement_mask, uint64_t statement,
                 trace_point **results_out, uint64_t *n_results_out);
void free_query_result(trace_point *results, uint64_t n_results);
void free_predicate(predicate *predicate);

typedef struct skip_list skip_list;

/* Create a skip list for tracking memory regions */
skip_list *create_memory_map();
void free_memory_map(skip_list *list);
/* Update memory map with buffer sizes from point.  */
void update_memory_map(skip_list *memory_map, const trace_point *point);
/* Compute size of buffer from memory map, storing the result into a
   var_info. */
void compute_buffer_size(const skip_list *memory_map,
                         const trace_read_state *state,
                         trace_var_info *var);

#ifdef __cplusplus
} // end extern "C"
#endif

#endif // __TRACE_DB_H
