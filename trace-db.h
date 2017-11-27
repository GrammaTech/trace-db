#ifndef __TRACE_DB_H
#define __TRACE_DB_H

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
    uint32_t n_traces_allocated;
} trace_db;

trace_db *create_db();
void collect_trace(trace_db *db, trace_read_state *state);
void free_db(trace_db *db);

#endif // __TRACE_DB_H
