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
void add_trace(trace_db *db, trace_read_state *state, uint64_t max);
void set_trace(trace_db *db, uint32_t index, trace *trace);
void free_db(trace_db *db);

typedef struct free_variable
{
    uint32_t n_allowed_types;
    /* Indices of compatible types */
    uint32_t *allowed_types;
} free_variable;

enum predicate_kind
{
    VAR_REFERENCE,
    VAR_SIZE,
    VAR_VALUE,
    SIGNED_INTEGER,
    UNSIGNED_INTEGER,
    AND,
    OR,
    DISTINCT_VARS,
    GREATER_THAN,
    LESS_THAN,
    EQUAL,
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

   *n_results_out will be set to the number of results. If the number of
   *results is non-zero, then *results_out will be set to an array of
   *results.

   The caller must pass the results to free_query_result to deallocate
   memory.
*/
void query_trace(const trace_db *db, uint64_t index,
                 uint32_t n_variables, const free_variable *variables,
                 const predicate *predicate,
                 trace_point **results_out, uint64_t *n_results_out);
void query_point(const trace_db *db, uint64_t trace_index, uint64_t point_index,
                 uint32_t n_variables, const free_variable *variables,
                 const predicate *predicate,
                 trace_point **results_out, uint64_t *n_results_out);
void free_query_result(trace_point *results, uint64_t n_results);
void free_predicate(predicate *predicate);

typedef struct skip_list skip_list;

skip_list *create_memory_map();
void free_memory_map(skip_list *list);
void update_memory_map(skip_list *memory_map, const trace_point *point);
void compute_buffer_size(const skip_list *memory_map,
                         const trace_read_state *state,
                         trace_var_info *var);

#ifdef __cplusplus
} // end extern "C"
#endif

#endif // __TRACE_DB_H
