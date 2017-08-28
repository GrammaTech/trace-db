#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "trace.h"

struct trace_read_state
{
    FILE *file;
    const char** dictionary;
};

trace_read_state *start_reading(const char *filename)
{
    trace_read_state *state = (trace_read_state *)malloc(sizeof(trace_read_state));
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
    state->dictionary = (const char**)malloc(sizeof(char*) * n_strings);
    char *buf = (char*)malloc(n_chars);;
    fread(buf, 1, n_chars, state->file);

    for (int i = 0; i < n_strings; i++) {
        state->dictionary[i] = buf; /* dictionary[0] points to original buf */
        while (*buf != 0)
            buf++;
        buf++;
    }
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
