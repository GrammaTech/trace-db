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
void free_db(trace_db *db);

typedef struct free_variable
{
    uint32_t n_allowed_types;
    /* Indices of compatible types */
    uint32_t *allowed_types;
} free_variable;

typedef struct query
{
    uint32_t n_variables;
    free_variable *variables;
} query;

/* Return all results which match query.

   Each result is represented as a trace point containing only the statement
   ID and bound variables.

   *n_results_out will be set to the number of results. If the number of
   *results is non-zero, then *results_out will be set to an array of
   *results.

   The caller must pass the results to free_query_result to deallocate
   memory.
*/
void query_trace(const trace_db *db, uint64_t index, const query *query,
                 trace_point **results_out, uint64_t *n_results_out);
void free_query_result(trace_point *results, uint64_t n_results);

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
