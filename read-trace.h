/* Functions for reading binary traces. */

#ifndef __READ_TRACE_H
#define __READ_TRACE_H

#include <stdint.h>
#include <stdio.h>
#include "types.h"

typedef struct trace_var_info
{
    union {
        uint64_t u;
        int64_t s;
        float f;
        double d;
        void *ptr;
    } value;
    uint16_t name_index;
    uint16_t type_index;
    uint16_t size;
} trace_var_info;

typedef struct trace_read_state
{
    FILE *file;
    const char **names;
    size_t n_names;
    const type_description *types;
    size_t n_types;

    trace_buffer_size *size_buffer;
    uint32_t n_sizes;
    trace_var_info *var_buffer;
    uint32_t n_vars;
} trace_read_state;

/*
Read header and initialize state.

The returned state should be cleaned up with end_reading().

Returns NULL if the header can't be read.
 */
trace_read_state *start_reading(const char *filename);

/*
   Read trace tag.

   Returns END_OF_TRACE at end of file, TRACE_TAG_ERROR for an unrecognized
   tag or any other error.
 */
enum trace_entry_tag read_tag(trace_read_state *state);

/*
   Read statement ID. Returns 0 on error.
 */
uint32_t read_id(trace_read_state *state);

/*
   Read a variable.

   Result will have size 0 on error.
 */
trace_var_info read_var_info(trace_read_state *state);

/*
   Read an address and corresponding buffer size.

   Result will have address 0 on error.
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
} trace_point;

/* Read an entire trace point.

   The resulting trace_point have pointers into caches in the state, and
   is only valid until the next call to read_trace_point.

   Return 0 if successful.
*/
int read_trace_point(trace_read_state *state, trace_point *result_ptr);

/* Read multiple trace points at once.

   The resulting trace_points have pointers into caches in the state, and are
   only valid until the next call to read_many_points.

   Return 0.
 */
int read_many_points(trace_read_state *state, trace_point *results, uint32_t limit);

#endif // __READ_TRACE_H
