/* Functions for reading binary traces. */

#ifndef __READ_TRACE_H
#define __READ_TRACE_H

#include <cstdint>
#include <cstdio>
#include "types.h"

extern "C" {

typedef struct trace_var_info
{
    union {
        uint64_t u;
        int64_t s;
        float f;
        double d;
        void *ptr;
    } value;
    uint32_t name_index;
    uint32_t type_index;
    uint32_t size;
    uint64_t buffer_size;
    uint8_t has_buffer_size;
} trace_var_info;

enum trace_error {
    TRACE_OK=0,
    TRACE_EOF,
    TRACE_ERROR
};

typedef struct trace_read_state
{
    FILE *file;
    const char **names;
    uint32_t n_names;
    const type_description *types;
    uint32_t n_types;

    trace_buffer_size *size_buffer;
    uint32_t n_sizes;
    trace_var_info *var_buffer;
    uint32_t n_vars;
    uint64_t *aux_buffer;
    uint32_t n_aux;

    enum trace_error error_code;
} trace_read_state;

/*
Read header and initialize state.

The returned state should be cleaned up with end_reading().

Returns NULL if the header can't be read or reading times out.
 */
trace_read_state *start_reading(const char *filename, int timeout_seconds);

/*
   Read trace tag.
 */
enum trace_entry_tag read_tag(trace_read_state *state);

/*
   Read statement ID.
 */
uint64_t read_id(trace_read_state *state);

/*
   Read a variable.

   If the variable format is BLOB, the result will contain a heap pointer to
   the value, which should be freed by the caller.
 */
trace_var_info read_var_info(trace_read_state *state);

/*
   Read an address and corresponding buffer size.
 */
trace_buffer_size read_buffer_size(trace_read_state *state);

void end_reading(trace_read_state *state);

typedef struct trace_point
{
    uint64_t statement;
    trace_buffer_size *sizes;
    uint32_t n_sizes;
    trace_var_info *vars;
    uint32_t n_vars;
    uint64_t *aux;
    uint32_t n_aux;
} trace_point;

/* Read an entire trace point.

   The resulting trace_point has pointers into caches in the state, and is
   only valid until the next call to read_trace_point.
*/
enum trace_error read_trace_point(trace_read_state *state, trace_point *result_ptr);

} // end extern "C"

#endif // __READ_TRACE_H
