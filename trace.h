/* Prototype of binary trace parsing.

   The goal at this point is to test performance and CFFI integration. The
   format is sufficient for our current use of traces in BugInjector but
   hasn't been designed with future needs in mind.

   The API is designed to minimize allocation. All data is returned on the
   stack so clients can process the trace as they read it, or build their own
   data structures, without any overhead from the API.

   No attempt has been made to handle errors or malformed traces.

   It remains to be seen whether this will work well with CFFI in
   practice. If there is a large amount of per-call overhead, it may be
   better to design an API which minimizes the number of function calls.

   The format consists of a dictionary (containing all string values used in
   the trace) followed by a series of trace entries.

   The dictionary is simply a sequence of NULL-terminated strings, followed
   by an additional NULL to mark the end of the list. Trace entries which
   refer to strings will use a one-byte index into this sequence.

   Each trace entry begins with a one byte tag indicating the type of entry
   (equivalent to the alist keys in Lisp traces). Further data depends on the
   type of entry. The structs below describe the layout of each entry type
   (although the on-disk structures are packed and the in-memory structs may
   have padding).

   A zero tag indicates the end of a trace point, and can be used to group
   related entries.

   An empty trace point (e.g. two zero tags in a row) indicates the end of
   the trace.
 */

#ifndef __TRACE_H
#define __TRACE_H

#include <stdint.h>

enum trace_entry_tag {
    END_ENTRY = 0,
    STATEMENT,
    SCOPES,
    SIZES
};

typedef struct trace_buffer_size
{
    uint64_t address;
    uint64_t size;
} trace_buffer_size;

typedef struct trace_var_info
{
    uint8_t name_index;
    uint8_t type_index;
    /* TODO: handle variables of different types */
    uint64_t value;
} trace_var_info;

typedef struct trace_read_state trace_read_state;

typedef struct trace_entry
{
    enum trace_entry_tag kind;
    uint64_t data;
} trace_entry;

trace_read_state *start_reading(const char *filename);

trace_entry read_entry(trace_read_state *state);
trace_var_info read_var_info(trace_read_state *state);
trace_buffer_size read_buffer_size(trace_read_state *state);

const char* string_lookup(trace_read_state *state, uint8_t index);

void end_reading(trace_read_state *state);

#endif // __TRACE_H
