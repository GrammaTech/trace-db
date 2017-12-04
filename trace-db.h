#ifndef __TRACE_DB_H
#define __TRACE_DB_H

#include "read-trace.h"

typedef struct trace
{
    trace_point *points;
    uint64_t n_points;
    /* XXX: should be 64 bits, causes type problems with
       ensure_buffer_size */
    uint32_t n_points_allocated;

    const char **names;
    uint32_t n_names;
    const type_description *types;
    uint32_t n_types;
} trace;

typedef struct trace_db
{
    trace *traces;
    uint64_t n_traces;
    uint32_t n_traces_allocated;
} trace_db;

trace_db *create_db();
void collect_trace(trace_db *db, trace_read_state *state);
void free_db(trace_db *db);

typedef struct skip_list skip_list;

skip_list *create_memory_map();
void free_memory_map(skip_list *list);
void update_memory_map(skip_list *memory_map, const trace_point *point);
void compute_buffer_size(const skip_list *memory_map,
                         const trace_read_state *state,
                         trace_var_info *var);

#endif // __TRACE_DB_H
