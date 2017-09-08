/* Types used for reading and writing */

#ifndef __TYPES_H
#define __TYPES_H

#include <stdint.h>

enum type_format {
    UNSIGNED,                   /* unsigned integer */
    SIGNED,                     /* signed integer */
    FLOAT,                      /* floating point */
    POINTER,                    /* unsigned, interpret as address */
    BLOB                        /* arbitrary bytes, do not interpret */
};

typedef struct {
    /* Index into the string dictionary which gives the name of the type. */
    uint16_t name_index;
    /* Data format */
    enum type_format format;
    /* Size in bytes. 0 indicates a variable-sized object. */
    uint8_t size;
} type_description;

enum trace_entry_tag {
    END_ENTRY = 0,
    STATEMENT_ID,
    VARIABLE,
    BUFFER_SIZE,
    UNKNOWN,
    /* Returned at EOF, should not appear in trace */
    END_OF_TRACE
};

typedef struct trace_buffer_size
{
    uint64_t address;
    uint64_t size;
} trace_buffer_size;


#endif // __TYPES_H
