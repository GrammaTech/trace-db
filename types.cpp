#include "types.h"

bool operator==(const type_description & a,
                const type_description & b) {
    return a.name_index == b.name_index &&
           a.format == b.format &&
           a.size == b.size;
}

bool operator==(const trace_buffer_size & a,
                const trace_buffer_size & b) {
    return a.address == b.address &&
           a.size == b.size;
}
