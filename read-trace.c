#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "read-trace.h"

#define FREAD_CHECK(ptr, size, nmemb, stream) \
    if (fread((ptr), (size), (nmemb), (stream)) != (nmemb)) goto error

trace_read_state *start_reading(const char *filename)
{
    trace_read_state *state = (trace_read_state *)calloc(1, sizeof(trace_read_state));
    state->file = fopen(filename, "rb");

    /* Read dictionary of names */
    uint16_t n_chars;
    FREAD_CHECK(&n_chars, sizeof(n_chars), 1, state->file);

    char *buf = (char*)malloc(n_chars);
    if (fread(buf, 1, n_chars, state->file) != n_chars) {
        free(buf);
        goto error;
    }

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
    FREAD_CHECK(&n_types, sizeof(n_types), 1, state->file);
    type_description *types = malloc(sizeof(type_description) * n_types);
    FREAD_CHECK(types, sizeof(type_description), n_types, state->file);
    state->types = types;
    state->n_types = n_types;

    for (int i = 0; i < n_types; i++) {
        if (types[i].format >= INVALID_FORMAT || types[i].name_index >= state->n_names)
            goto error;
    }

    return state;

 error:
    if (state)
        end_reading(state);

    return NULL;
}

enum trace_entry_tag read_tag(trace_read_state *state)
{
    char c = fgetc(state->file);
    if (c == EOF)
        return END_OF_TRACE;
    else if (c >= TRACE_TAG_ERROR)
        return TRACE_TAG_ERROR;

    return (enum trace_entry_tag)c;
}

uint32_t read_id(trace_read_state *state)
{
    uint32_t id;
    FREAD_CHECK(&id, sizeof(id), 1, state->file);

    return id;

 error:
    return 0;
}

trace_var_info read_var_info(trace_read_state *state)
{
    trace_var_info result;
    FREAD_CHECK(&result.name_index, sizeof(result.name_index), 1, state->file);
    FREAD_CHECK(&result.type_index, sizeof(result.type_index), 1, state->file);

    if (result.name_index >= state->n_names || result.type_index > state->n_types)
        goto error;

    type_description type = state->types[result.type_index];
    if (type.format == BLOB) {
        /* Blob type: store value on the heap */
        uint16_t size = type.size;
        if (type.size == 0) {
            /* Variable-sized type: read size from trace */
            FREAD_CHECK(&size, sizeof(size), 1, state->file);
        }
        result.size = size;

        result.value.ptr = malloc(size);
        FREAD_CHECK(result.value.ptr, size, 1, state->file);
    }
    else {
        /* Primitive type: read directly into result */
        result.size = type.size;
        result.value.u = 0;
        FREAD_CHECK(&result.value.u, result.size, 1, state->file);
    }

    return result;

 error:
    result.size = 0;
    return result;
}

trace_buffer_size read_buffer_size(trace_read_state *state)
{
    trace_buffer_size result;
    FREAD_CHECK(&result, sizeof(result), 1, state->file);

    return result;

 error:
    result.address = 0;
    result.size = 0;
    return result;
}

void end_reading(trace_read_state *state)
{
    if (state->file)
        fclose(state->file);
    if (state->names) {
        if (state->n_names > 0)
            free((void *)state->names[0]);
        free(state->names);
    }
    if (state->types)
        free((void *)state->types);
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

int read_trace_point(trace_read_state *state, trace_point *result_ptr){
    trace_point result = { 0, 0, 0, 0 };

    while (1) {
        enum trace_entry_tag tag = read_tag(state);
        if (tag == TRACE_TAG_ERROR || tag == END_OF_TRACE)
            goto error;
        if (tag == END_OF_TRACE)
            break;

        switch (tag) {
        case END_ENTRY:
          goto end;
        case STATEMENT_ID:
          result.statement = read_id(state);
          if (result.statement == 0)
              goto error;
          break;
        case VARIABLE:
            {
                trace_var_info info = read_var_info(state);
                if (info.size == 0)
                    goto error;

                result.n_vars++;
                ensure_buffer_size((void **)&(state->var_buffer), sizeof(trace_var_info),
                                   &state->n_vars, result.n_vars);
                state->var_buffer[result.n_vars - 1] = info;

                break;
            }

        case BUFFER_SIZE:
            {
                trace_buffer_size info = read_buffer_size(state);
                if (info.address == 0)
                    goto error;


                result.n_sizes++;
                ensure_buffer_size((void **)&(state->size_buffer), sizeof(trace_buffer_size),
                                   &state->n_sizes, result.n_sizes);
                state->size_buffer[result.n_sizes - 1] = info;
                break;
            }
        default:
          goto error;
        }
    }

 end:
    result.vars = state->var_buffer;
    result.sizes = state->size_buffer;
    *result_ptr = result;
    return (result.statement > 0) ? 0 : 1;

 error:
    return -1;
}
