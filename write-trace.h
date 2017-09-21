/* Functions for writing binary traces. */

#ifndef __WRITE_TRACE_H
#define __WRITE_TRACE_H

#include <stdint.h>
#include "types.h"

#ifdef __cplusplus
# define AUTO auto
#else
# define AUTO __auto_type
#endif

#define WRITE_TRACE_VARIABLE(out, name_index, type_index, var) \
    do {                                                       \
        uint16_t val;                                          \
        fputc(VARIABLE, out);                                  \
        val = name_index; fwrite(&val, sizeof(val), 1, out);   \
        val = type_index; fwrite(&val, sizeof(val), 1, out);   \
        /* Assigning to tmp here solves two problems:
           1. If var has the "register" storage class, we can't take its address,
              but tmp will discard the storage class.
           2. If var is a static array, sizeof() will give the size of its
              contents when we want to write its address. But tmp will be
              a pointer.
        */                                                     \
        AUTO tmp = var;                                        \
        fwrite(&tmp, sizeof(tmp), 1, out);                     \
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

void write_trace_header(FILE *out, const char **names, uint16_t n_names,
                        const type_description *types, uint16_t n_types);
void write_trace_id(FILE *out, uint32_t statement_id);
void write_trace_aux(FILE *out, uint64_t value);
void write_end_entry(FILE *out);
void write_buffer_size(FILE *out, void *address, size_t size);

#endif // __WRITE_TRACE_H
