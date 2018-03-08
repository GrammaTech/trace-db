#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "utils.h"

#include "read-trace.h"

#define FREAD_CHECK(ptr, size, nmemb, state   )                                  \
    do {                                                                         \
        if (fread((ptr), (size), (nmemb), (state)->file) != (nmemb)) {           \
            (state)->error_code = feof((state)->file) ? TRACE_EOF : TRACE_ERROR; \
            goto error;                                                          \
        }                                                                        \
    } while (0)

trace_read_state *start_reading(const char *filename)
{
    trace_read_state *state = (trace_read_state *)calloc(1, sizeof(trace_read_state));
    int fd = open(filename, O_RDONLY);

    state->file = fdopen(fd, "rb");
    if (!state->file)
        goto error;

    /* Read dictionary of names */
    uint64_t n_chars;
    FREAD_CHECK(&n_chars, sizeof(n_chars), 1, state);

    char *buf = (char*)malloc(n_chars);
    if (fread(buf, 1, n_chars, state->file) != n_chars) {
        free(buf);
        goto error;
    }

    /* Scan once to count strings */
    uint32_t n_strings = 0;
    for (uint32_t i = 0; i < n_chars; i++) {
        if (buf[i] == 0)
            n_strings++;
    }
    state->names = (const char**)malloc(sizeof(char*) * n_strings);

    /* Scan again to find starts of each string */
    for (uint32_t i = 0; i < n_strings; i++) {
        state->names[i] = buf; /* names[0] points to original buf */
        while (*buf != 0)
            buf++;
        buf++;
    }
    state->n_names = n_strings;

    /* Read dictionary of types */
    uint32_t n_types;
    FREAD_CHECK(&n_types, sizeof(n_types), 1, state);
    type_description *types = malloc(sizeof(type_description) * n_types);
    FREAD_CHECK(types, sizeof(type_description), n_types, state);
    state->types = types;
    state->n_types = n_types;

    for (uint32_t i = 0; i < n_types; i++) {
        if (types[i].format >= INVALID_FORMAT || types[i].name_index >= state->n_names)
            goto error;
    }

    state->error_code = TRACE_OK;
    return state;

 error:
    if (state)
        end_reading(state);

    return NULL;
}

enum trace_entry_tag read_tag(trace_read_state *state)
{
    char c = fgetc(state->file);
    if (c == EOF) {
        state->error_code = TRACE_EOF;
    }
    else if (c >= INVALID_TAG) {
        state->error_code = TRACE_ERROR;
    }

    return (enum trace_entry_tag)c;
}

uint64_t read_id(trace_read_state *state)
{
    uint64_t id;
    FREAD_CHECK(&id, sizeof(id), 1, state);

    return id;

 error:
    return 0;
}

trace_var_info read_var_info(trace_read_state *state)
{
    trace_var_info result;
    result.has_buffer_size = 0;

    FREAD_CHECK(&result.name_index, sizeof(result.name_index), 1, state);
    FREAD_CHECK(&result.type_index, sizeof(result.type_index), 1, state);

    if (result.name_index >= state->n_names
        || result.type_index >= state->n_types) {
        state->error_code = TRACE_ERROR;
        goto error;
    }

    type_description type = state->types[result.type_index];

    result.size = type.size;
    result.value.u = 0;

    switch (type.format) {
    case SIGNED:
        {
            switch (result.size) {
            case 1:
                {
                    int8_t tmp;
                    FREAD_CHECK(&tmp, result.size, 1, state);
                    result.value.s = tmp;
                }
                break;
            case 2:
                {
                    int16_t tmp;
                    FREAD_CHECK(&tmp, result.size, 1, state);
                    result.value.s = tmp;
                }
                break;
            case 4:
                {
                    int32_t tmp;
                    FREAD_CHECK(&tmp, result.size, 1, state);
                    result.value.s = tmp;
                }
                break;
            case 8:
                {
                    int64_t tmp;
                    FREAD_CHECK(&tmp, result.size, 1, state);
                    result.value.s = tmp;
                }
                break;
            default:
                goto error;
            }
        }
        break;
    case UNSIGNED:
    case FLOAT:
    case POINTER:
        FREAD_CHECK(&result.value.u, result.size, 1, state);
        break;
    case BLOB:
        {
            /* Blob type: store value on the heap */
            uint32_t size = result.size;
            if (size == 0) {
                /* Variable-sized type: read size from trace */
                FREAD_CHECK(&size, sizeof(size), 1, state);
            }
            result.size = size;

            result.value.ptr = malloc(size);
            FREAD_CHECK(result.value.ptr, size, 1, state);
            break;
        }
    case INVALID_FORMAT:
    default:
        goto error;
    }

 error:
    return result;
}

trace_buffer_size read_buffer_size(trace_read_state *state)
{
    trace_buffer_size result;
    FREAD_CHECK(&result, sizeof(result), 1, state);

 error:
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
    if (state->size_buffer)
        free(state->size_buffer);
    if (state->var_buffer)
        free(state->var_buffer);
    if (state->aux_buffer)
        free(state->aux_buffer);
    free(state);
}

enum trace_error read_trace_point(trace_read_state *state, trace_point *result_ptr)
{
    trace_point result = { 0, 0, 0, 0 };

    while (1) {
        enum trace_entry_tag tag = read_tag(state);
        if (state->error_code != TRACE_OK)
            goto error;

        switch (tag) {
        case END_ENTRY:
            goto end;
        case STATEMENT_ID:
            result.statement = read_id(state);
            if (state->error_code != TRACE_OK)
                goto error;
            break;
        case VARIABLE:
            {
                trace_var_info info = read_var_info(state);
                if (state->error_code != TRACE_OK)
                    goto error;

                result.n_vars++;
                ENSURE_BUFFER_SIZE(state->var_buffer, sizeof(trace_var_info),
                                   state->n_vars, result.n_vars);
                state->var_buffer[result.n_vars - 1] = info;

                break;
            }

        case BUFFER_SIZE:
            {
                trace_buffer_size info = read_buffer_size(state);
                if (state->error_code != TRACE_OK)
                    goto error;

                result.n_sizes++;
                ENSURE_BUFFER_SIZE(state->size_buffer, sizeof(trace_buffer_size),
                                   state->n_sizes, result.n_sizes);
                state->size_buffer[result.n_sizes - 1] = info;
                break;
            }
        case AUXILIARY:
            {
                uint64_t value;
                FREAD_CHECK(&value, sizeof(value), 1, state);

                result.n_aux++;
                ENSURE_BUFFER_SIZE(state->aux_buffer, sizeof(uint64_t),
                                   state->n_aux, result.n_aux);
                state->aux_buffer[result.n_aux - 1] = value;
                break;
            }
        default:
            goto error;
        }
    }

 end:
    result.vars = state->var_buffer;
    result.sizes = state->size_buffer;
    result.aux = state->aux_buffer;
    *result_ptr = result;
 error:
    return state->error_code;
}
