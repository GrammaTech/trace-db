#include <stdlib.h>

#include "read-trace.h"
#include "utils.h"

#include "trace-db.h"

trace_db *create_db()
{
    return calloc(1, sizeof(trace_db));
}

#define INITIAL_TRACE_SIZE (1 << 16)

void collect_trace(trace_db *db, trace_read_state *state)
{
    trace trace = { malloc(INITIAL_TRACE_SIZE * sizeof(trace_point)),
                    0, INITIAL_TRACE_SIZE };

    /* Read trace points */
    trace_point point;
    while (read_trace_point(state, &point) == 0) {
        /* Copy contents out of shared state buffers */
        point.sizes = malloc_copy(point.sizes,
                                  point.n_sizes * sizeof(trace_buffer_size));
        point.vars = malloc_copy(point.vars,
                                 point.n_vars * sizeof(trace_var_info));
        point.aux = malloc_copy(point.aux, point.n_aux * sizeof(*point.aux));

        ENSURE_BUFFER_SIZE(trace.points, sizeof(trace_point),
                           trace.n_points_allocated, trace.n_points + 1);
        trace.points[trace.n_points++] = point;
    }

    /* Move names and types from state into trace */
    trace.names = state->names;
    trace.n_names = state->n_names;
    state->names = NULL;

    trace.types = state->types;
    trace.n_types = state->n_types;
    state->types = NULL;

    end_reading(state);

    /* Store trace */
    ENSURE_BUFFER_SIZE(db->traces, sizeof(trace),
                       db->n_traces_allocated, db->n_traces + 1);
    db->traces[db->n_traces++] = trace;
}

void free_db(trace_db *db)
{
    for (int i = 0; i < db->n_traces; i++) {
        if (db->traces[i].n_points > 0)
            free(db->traces[i].points);
        free(db->traces);
    }
    free(db);
}
