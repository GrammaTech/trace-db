#include <cassert>
#include <cstdarg>
#include <cstdio>
#include <cstring>

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

void write_trace_variables(FILE *out, uint32_t n_vars, ...)
{
    va_list ap;
    uint32_t i;

    va_start (ap, n_vars);
    for (i = 0; i < n_vars; i++) {
        uint32_t name_index = va_arg(ap, uint32_t);
        uint32_t type_index = va_arg(ap, uint32_t);
        uint32_t size = va_arg(ap, uint32_t);
        enum type_format format = (enum type_format)va_arg(ap, int);

        fputc(VARIABLE, out);
        fwrite(&name_index, sizeof(name_index), 1, out);
        fwrite(&type_index, sizeof(type_index), 1, out);

        /* This code is tricky because va_args are subject to standard
         promotions: smaller integers become ints, and floats become
         doubles. Other types are left alone.
        */
        switch (format) {
        case UNSIGNED:
        case SIGNED:
          switch (size) {
          case 1:
              {
                  char val = va_arg(ap, int);
                  fwrite(&val, sizeof(val), 1, out);
                  break;
              }
          case 2:
              {
                  int16_t val = va_arg(ap, int);
                  fwrite(&val, sizeof(val), 1, out);
                  break;
              }
          case 4:
              {
                  int32_t val = va_arg(ap, int);
                  fwrite(&val, sizeof(val), 1, out);
                  break;
              }
          case 8:
              {
                  int64_t val = va_arg(ap, int64_t);
                  fwrite(&val, sizeof(val), 1, out);
                  break;
              }
          }
          break;
        case FLOAT:
          if (size == 4) {
              float val = (float)va_arg(ap, double);
              fwrite(&val, sizeof(val), 1, out);
              break;
          }
          else {
              double val = va_arg(ap, double);
              fwrite(&val, sizeof(val), 1, out);
              break;
          }
          break;
        case POINTER:
            {
                void *val = va_arg(ap, void*);
                fwrite(&val, sizeof(val), 1, out);
            }
            break;
        case BLOB:
        case INVALID_FORMAT:
        default:
          break;
        }
    }
}

void write_trace_blobs(FILE *out, uint32_t n_vars, ...)
{
    va_list ap;
    uint32_t i;

    va_start (ap, n_vars);
    for (i = 0; i < n_vars; i++) {
        uint32_t name_index = va_arg(ap, uint32_t);
        uint32_t type_index = va_arg(ap, uint32_t);
        uint32_t size = va_arg(ap, uint32_t);
        void *value = va_arg(ap, void*);

        fputc(VARIABLE, out);
        fwrite(&name_index, sizeof(name_index), 1, out);
        fwrite(&type_index, sizeof(type_index), 1, out);
        fwrite(&size, sizeof(size), 1, out);
        fwrite(value, size, 1, out);
    }
}
