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

/* Assigning to tmp here solves two problems:
   1. If var has the "register" storage class, we can't take its address,
   but tmp will discard the storage class.
   2. If var is a static array, sizeof() will give the size of its
   contents when we want to write its address. But tmp will be
   a pointer.
*/
#define WRITE_TRACE_VARIABLE(out, name_index, type_index, var) \
    do {                                                       \
        AUTO tmp = var;                                        \
        write_trace_variable(out, name_index, type_index,      \
                             sizeof(tmp), &tmp);               \
    } while(0)

void write_trace_header(FILE *out, const char **names, uint32_t n_names,
                        const type_description *types, uint32_t n_types);
void write_trace_id(FILE *out, uint64_t statement_id);
void write_trace_aux(FILE *out, uint64_t value);
void write_end_entry(FILE *out);
void write_buffer_size(FILE *out, void *address, size_t size);
void write_trace_variable(FILE *out, uint32_t name_index, uint32_t type_index,
                          uint32_t size, void *var);
void write_trace_blob(FILE *out, uint32_t name_index, uint32_t type_index,
                      uint32_t size, void *var);

#endif // __WRITE_TRACE_H
