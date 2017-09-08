#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "read-trace.h"

trace_read_state *start_reading(const char *filename)
{
    trace_read_state *state = (trace_read_state *)malloc(sizeof(trace_read_state));
    state->size_buffer = NULL;
    state->n_sizes = 0;
    state->var_buffer = NULL;
    state->n_vars = 0;
    state->file = fopen(filename, "rb");

    /* Read dictionary of names */
    uint16_t n_chars;
    fread(&n_chars, sizeof(n_chars), 1, state->file);

    char *buf = (char*)malloc(n_chars);
    fread(buf, 1, n_chars, state->file);

    /* Scan once to count strings */
    uint16_t n_strings = 0;
    for (int i = 0; i < n_chars; i++) {
        if (buf[i] == 0)
            n_strings++;
    }
    state->names = (const char**)malloc(sizeof(char*) * n_strings);

    /* Scan again to find starts of each string */
    for (int i = 0; i < n_strings; i++) {
        state->names[i] = buf; /* names[0] points to original buf */
        while (*buf != 0)
            buf++;
        buf++;
    }
    state->n_names = n_strings;

    /* Read dictionary of types */
    uint16_t n_types;
    fread(&n_types, sizeof(n_types), 1, state->file);
    type_description *types = malloc(sizeof(type_description) * n_types);
    fread(types, sizeof(type_description), n_types, state->file);
    state->types = types;
    state->n_types = n_types;

    return state;
}

enum trace_entry_tag read_tag(trace_read_state *state)
{
    char c = fgetc(state->file);
    if (c == EOF)
        return END_OF_TRACE;
    assert(c < UNKNOWN);

    return (enum trace_entry_tag)c;
}

uint32_t read_id(trace_read_state *state)
{
    uint32_t id;
    fread(&id, sizeof(id), 1, state->file);

    return id;
}

trace_var_info read_var_info(trace_read_state *state)
{
    trace_var_info result;
    fread(&result.name_index, sizeof(result.name_index), 1, state->file);
    fread(&result.type_index, sizeof(result.type_index), 1, state->file);

    type_description type = state->types[result.type_index];
    if (type.format == BLOB) {
        /* Blob type: store value on the heap */
        uint16_t size = type.size;
        if (type.size == 0) {
            /* Variable-sized type: read size from trace */
            fread(&size, sizeof(size), 1, state->file);
        }
        result.size = size;

        result.value.ptr = malloc(size);
        fread(result.value.ptr, size, 1, state->file);
    }
    else {
        /* Primitive type: read directly into result */
        result.size = type.size;
        result.value.u = 0;
        fread(&result.value.u, result.size, 1, state->file);
    }

    return result;
}

trace_buffer_size read_buffer_size(trace_read_state *state)
{
    trace_buffer_size result;
    fread(&result, sizeof(result), 1, state->file);

    return result;
}

void end_reading(trace_read_state *state)
{
    fclose(state->file);
    free((void *)state->names[0]);
    free(state->names);
    free(state);
}

#if 0
static void ensure_buffer_size(void **buffer, size_t element_size,
                               uint32_t *allocated, uint32_t needed)
{
    if (*allocated >= needed)
        return;

    if (*allocated == 0)
        *allocated = 1024;
    else
        *allocated *= 2;

    *buffer = realloc(*buffer, *allocated * element_size);
}

int read_trace_point(trace_read_state *state, trace_point *result_ptr)
{
    trace_point result = { 0, 0, 0, 0 };
    int valid_point = 0;

    while (1) {
        trace_entry entry = read_entry(state);

        switch (entry.kind) {
        case END_ENTRY:
          goto end;
        case STATEMENT_ID:
          result.statement = entry.data;
          break;
        case VARIABLE:
          result.n_vars = entry.data;
          ensure_buffer_size((void **)&(state->var_buffer), sizeof(trace_var_info),
                             &state->n_vars, result.n_vars);
          for (size_t i = 0; i < result.n_vars; i++) {
              state->var_buffer[i] = read_var_info(state);
          }
          result.vars = state->var_buffer;
          break;
        case SIZES:
          result.n_sizes = entry.data;
          ensure_buffer_size((void **)&(state->size_buffer), sizeof(trace_buffer_size),
                         &state->n_sizes, result.n_sizes);
          for (size_t i = 0; i < result.n_sizes; i++) {
              state->size_buffer[i] = read_buffer_size(state);
          }
          result.sizes = state->size_buffer;
        }
        valid_point = 1 ;
    }

 end:
    *result_ptr = result;
    return valid_point ? 0 : 1;
}

int read_many_points(trace_read_state *state, trace_point *results, uint32_t limit)
{
    uint32_t var_index = 0, size_index = 0;
    uint32_t *var_start = malloc(limit * sizeof(uint32_t));
    uint32_t *size_start = malloc(limit * sizeof(uint32_t));

    uint32_t n_read;
    for (n_read = 0; n_read < limit; n_read++) {
        trace_point result = { 0, 0, 0, 0 };
        int valid_point = 0;

        while (1) {
            trace_entry entry = read_entry(state);

            switch (entry.kind) {
            case END_ENTRY:
              goto end_point;
            case STATEMENT_ID:
              result.statement = entry.data;
              break;
            case VARIABLE:
              result.n_vars = entry.data;
              var_start[n_read] = var_index;

              ensure_buffer_size((void **)&(state->var_buffer), sizeof(trace_var_info),
                                 &state->n_vars, var_index + result.n_vars);
              for (size_t i = 0; i < result.n_vars; i++) {
                  state->var_buffer[var_index++] = read_var_info(state);
              }
              break;
            case SIZES:
              result.n_sizes = entry.data;
              size_start[n_read] = size_index;

              ensure_buffer_size((void **)&(state->size_buffer), sizeof(trace_buffer_size),
                                 &state->n_sizes, size_index + result.n_sizes);
              for (size_t i = 0; i < result.n_sizes; i++) {
                  state->size_buffer[size_index++] = read_buffer_size(state);
              }
              break;
            }
            valid_point = 1 ;
        }

    end_point:
        if (valid_point)
            results[n_read] = result;
        else
            break;              /* end of trace */
    }

    /* var and size buffers may have been realloced while reading. Convert
       buffer indices into pointers for all points. */
    for (uint32_t i = 0; i < n_read; i++) {
        if (results[i].n_vars > 0)
            results[i].vars = state->var_buffer + var_start[i];
        if (results[i].n_sizes > 0)
            results[i].sizes = state->size_buffer + size_start[i];
    }

    free(var_start);
    free(size_start);

    return n_read;
}
#endif
