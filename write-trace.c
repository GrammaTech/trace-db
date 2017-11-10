#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "write-trace.h"

void write_trace_header(FILE *out, const char **names, uint32_t n_names,
                        const type_description *types, uint32_t n_types)
{
    /* Dictionary of names, as a sequence of NULL-terminated strings */
    uint64_t total_size = 0;
    for (uint64_t i = 0; i < n_names; i++) {
        total_size += strlen(names[i]) + 1;
    }
    fwrite(&total_size, sizeof(total_size), 1, out);

    for (uint32_t i = 0; i < n_names; i++) {
        fputs(names[i], out);
        fputc(0, out);
    }

    /* Dictionary of types */
    fwrite(&n_types, sizeof(n_types), 1, out);
    fwrite(types, sizeof(*types), n_types, out);
}

void write_trace_id(FILE *out, uint64_t statement_id)
{
    assert(statement_id != 0);
    fputc(STATEMENT_ID, out);
    fwrite(&statement_id, sizeof(statement_id), 1, out);
}

void write_trace_aux(FILE *out, uint64_t value)
{
    fputc(AUXILIARY, out);
    fwrite(&value, sizeof(value), 1, out);
}

void write_end_entry(FILE *out)
{
    fputc(END_ENTRY, out);
    fflush(out);
}

void write_buffer_size(FILE *out, void *address, size_t size)
{
    fputc(BUFFER_SIZE, out);
    trace_buffer_size val = { (uint64_t)address, (uint64_t)size };
    fwrite(&val, sizeof(val), 1, out);
}

void write_trace_variable(FILE *out, uint32_t name_index, uint32_t type_index,
                          uint32_t size, void *var)
{
    fputc(VARIABLE, out);
    fwrite(&name_index, sizeof(name_index), 1, out);
    fwrite(&type_index, sizeof(type_index), 1, out);
    fwrite(var, size, 1, out);
}

void write_trace_blob(FILE *out, uint32_t name_index, uint32_t type_index,
                      uint32_t size, void *var)
{
    fputc(VARIABLE, out);
    fwrite(&name_index, sizeof(name_index), 1, out);
    fwrite(&type_index, sizeof(type_index), 1, out);
    fwrite(&size, sizeof(size), 1, out);
    fwrite(var, size, 1, out);
}
