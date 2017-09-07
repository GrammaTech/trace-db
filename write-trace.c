#include <stdio.h>
#include <string.h>

#include "write-trace.h"

void write_trace_header(FILE *out, const char **names, size_t n_names,
                        const type_description *types, size_t n_types)
{
    /* Dictionary of names, as a sequence of NULL-terminated strings */
    size_t total_size = 0;
    for (size_t i = 0; i < n_names; i++) {
        total_size += strlen(names[i]) + 1;
    }
    fwrite(&total_size, sizeof(total_size), 1, out);

    for (size_t i = 0; i < n_names; i++) {
        fputs(names[i], out);
        fputc(0, out);
    }

    /* Dictionary of types */
    fwrite(&n_types, sizeof(n_types), 1, out);
    fwrite(types, sizeof(*types), n_types, out);
}

void write_trace_id(FILE *out, uint64_t statement_id)
{
    fputc(STATEMENT_ID, out);
    fwrite(&statement_id, sizeof(statement_id), 1, out);
}

void write_end_entry(FILE *out)
{
    fputc(0, out);
}

void write_buffer_size(FILE *out, void *address, size_t size)
{
    fputc(BUFFER_SIZE, out);
    fwrite(&address, sizeof(address), 1, out);
    fwrite(&size, sizeof(size), 1, out);
}

#define WRITE_TRACE_VARIABLE(out, name_index, type_index, var)        \
    do {                                                              \
        uint16_t val;                                                 \
        fputc(VARIABLE, out);                                         \
        val = name_index; fwrite(&val, sizeof(val), 1, out);          \
        val = type_index; fwrite(&val, sizeof(val), 1, out);          \
        fwrite(&var, sizeof(var), 1, out);                            \
    } while(0)

#define WRITE_TRACE_BLOB(out, name_index, type_index, size, ptr)        \
    do {                                                                \
        uint16_t val;                                                   \
        fputc(VARIABLE, out);                                           \
        val = name_index; fwrite(&val, sizeof(val), 1, out);            \
        val = type_index; fwrite(&val, sizeof(val), 1, out);            \
        val = size; fwrite(&val, sizeof(val), 1, out);                  \
        fwrite(ptr, size, 1, out);                                      \
    } while (0)
