#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "trace.h"

trace_read_state *start_reading(const char *filename)
{
    trace_read_state *state = (trace_read_state *)malloc(sizeof(trace_read_state));
    state->size_buffer = NULL;
    state->n_sizes = 0;
    state->var_buffer = NULL;
    state->n_vars = 0;
    state->file = fopen(filename, "rb");

    /* Scan through the string dictionary, counting characters and strings */
    /* XXX: a size or count field would simplify this */
    size_t n_chars, n_strings = 0;
    int non_empty = 0;
    for (n_chars = 0;; n_chars++) {
        if (fgetc(state->file) == 0) {
            if (non_empty) {     /* end of string */
                non_empty = 0;
                n_strings++;
            }
            else                /* empty string terminates dictionary */
                break;
        } else {
            non_empty = 1;
        }
    }

    /* Now read the whole dictionary and split it up */
    rewind(state->file);
    state->dictionary = (const char**)malloc(sizeof(char*) * (n_strings + 1));
    char *buf = (char*)malloc(n_chars);;
    fread(buf, 1, n_chars, state->file);

    for (int i = 0; i < n_strings; i++) {
        state->dictionary[i] = buf; /* dictionary[0] points to original buf */
        while (*buf != 0)
            buf++;
        buf++;
    }
    state->dictionary[n_strings] = NULL;
    /* Read final NULL which terminates dictionary */
    assert(fgetc(state->file) == 0);

    return state;
}

trace_entry read_entry(trace_read_state *state)
{
    trace_entry result;
    result.kind = fgetc(state->file);
    if (result.kind == EOF)
        result.kind = END_ENTRY;

    /* No data */
    if (result.kind == END_ENTRY)
        return result;

    /* FIXME: handle EOF here (in case of truncated trace) */
    if (result.kind == STATEMENT)
        fread(&result.data, sizeof(result.data), 1, state->file);
    else {
        /* Just a single byte var/size counts */
        result.data = fgetc(state->file);
    }

    return result;
}

trace_var_info read_var_info(trace_read_state *state)
{
    trace_var_info result;
    result.name_index = fgetc(state->file);
    result.type_index = fgetc(state->file);
    fread(&result.value, 1, sizeof(result.value), state->file);

    return result;
}

trace_buffer_size read_buffer_size(trace_read_state *state)
{
    trace_buffer_size result;
    fread(&result, sizeof(result), 1, state->file);

    return result;
}

const char* string_lookup(trace_read_state *state, uint8_t index)
{
    return state->dictionary[index];
}

void end_reading(trace_read_state *state)
{
    fclose(state->file);
    free((void *)state->dictionary[0]);
    free(state->dictionary);
    free(state);
}

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
        case STATEMENT:
          result.statement = entry.data;
          break;
        case SCOPES:
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
            case STATEMENT:
              result.statement = entry.data;
              break;
            case SCOPES:
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
