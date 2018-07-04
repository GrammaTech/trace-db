/* Functions for reading binary traces. */

#ifndef __READ_TRACE_H
#define __READ_TRACE_H

#include <cassert>
#include <cstdint>
#include <cstdio>

#include <boost/archive/basic_archive.hpp>

#include "types.h"

extern "C" {

typedef struct trace_var_info
{
    union {
        uint64_t u;
        int64_t s;
        float f;
        double d;
        void *ptr;
    } value;
    uint32_t name_index;
    uint32_t type_index;
    uint32_t size;
    uint64_t buffer_size;
    uint8_t has_buffer_size;
} trace_var_info;

enum trace_error {
    TRACE_OK=0,
    TRACE_EOF,
    TRACE_ERROR
};

typedef struct trace_read_state
{
    FILE *file;
    char **names;
    uint32_t n_names;
    type_description *types;
    uint32_t n_types;

    trace_buffer_size *size_buffer;
    uint32_t n_sizes;
    trace_var_info *var_buffer;
    uint32_t n_vars;
    uint64_t *aux_buffer;
    uint32_t n_aux;

    enum trace_error error_code;
} trace_read_state;

/*
Read header and initialize state.

The returned state should be cleaned up with end_reading().

Returns NULL if the header can't be read or reading times out.
 */
trace_read_state *start_reading(const char *filename, int timeout_seconds);

/*
   Read trace tag.
 */
enum trace_entry_tag read_tag(trace_read_state *state);

/*
   Read statement ID.
 */
uint64_t read_id(trace_read_state *state);

/*
   Read a variable.

   If the variable format is BLOB, the result will contain a heap pointer to
   the value, which should be freed by the caller.
 */
trace_var_info read_var_info(trace_read_state *state);

/*
   Read an address and corresponding buffer size.
 */
trace_buffer_size read_buffer_size(trace_read_state *state);

void end_reading(trace_read_state *state);

typedef struct trace_point
{
    uint64_t statement;
    trace_buffer_size *sizes;
    uint32_t n_sizes;
    trace_var_info *vars;
    uint32_t n_vars;
    uint64_t *aux;
    uint32_t n_aux;
} trace_point;

/* Read an entire trace point.

   The resulting trace_point has pointers into caches in the state, and is
   only valid until the next call to read_trace_point.
*/
enum trace_error read_trace_point(trace_read_state *state, trace_point *result_ptr);

} // end extern "C"

bool operator==(const trace_var_info &a,
                const trace_var_info &b);
bool operator==(const trace_point &a,
                const trace_point &b);

namespace boost {
namespace serialization {

template<class Archive>
void serialize(Archive & ar,
               trace_var_info & var_info,
               const unsigned int version) {
    ar & var_info.value.u;
    ar & var_info.name_index;
    ar & var_info.type_index;
    ar & var_info.size;
    ar & var_info.buffer_size;
    ar & var_info.has_buffer_size;
}

template<class Archive>
void save(Archive & ar,
          const trace_point & trace_point,
          const unsigned int version) {
    ar & trace_point.statement;
    ar & trace_point.n_sizes;
    ar & trace_point.n_vars;
    ar & trace_point.n_aux;

    for (uint32_t i = 0; i < trace_point.n_sizes; i++)
        ar & trace_point.sizes[i];

    for (uint32_t i = 0; i < trace_point.n_vars; i++)
        ar & trace_point.vars[i];

    for (uint32_t i = 0; i < trace_point.n_aux; i++)
        ar & trace_point.aux[i];
}

template<class Archive>
void load(Archive & ar,
          trace_point & trace_point,
          unsigned int version) {
    ar & trace_point.statement;
    ar & trace_point.n_sizes;
    ar & trace_point.n_vars;
    ar & trace_point.n_aux;

    assert(trace_point.n_sizes >= 0);
    assert(trace_point.n_vars >= 0);
    assert(trace_point.n_aux >= 0);

    trace_point.sizes = NULL;
    trace_point.vars = NULL;
    trace_point.aux = NULL;

    if (trace_point.n_sizes > 0)
        trace_point.sizes =
            (trace_buffer_size*) calloc(trace_point.n_sizes,
                                        sizeof(trace_buffer_size));
    if (trace_point.n_vars > 0)
        trace_point.vars =
            (trace_var_info*) calloc(trace_point.n_vars,
                                     sizeof(trace_var_info));
    if (trace_point.n_aux > 0)
        trace_point.aux =
            (uint64_t*) calloc(trace_point.n_aux,
                               sizeof(uint64_t));

    for (uint32_t i = 0; i < trace_point.n_sizes; i++)
        ar & trace_point.sizes[i];

    for (uint32_t i = 0; i < trace_point.n_vars; i++)
        ar & trace_point.vars[i];

    for (uint32_t i = 0; i < trace_point.n_aux; i++)
        ar & trace_point.aux[i];
}


template<class Archive>
void serialize(Archive & ar,
               trace_point & trace_point,
               const unsigned int file_version) {
    split_free(ar, trace_point, file_version);
}

} // end namespace serialization
} // end namespace boost

#endif // __READ_TRACE_H
