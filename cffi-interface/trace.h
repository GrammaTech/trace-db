//
// trace.h -- C interface for reading and retrieving information
// on binary traces.
//

#ifndef __TRACE_H
#define __TRACE_H

#include <cstdint>

#include "Trace.hpp"
#include "TracePoint.hpp"
#include "TraceVarInfo.hpp"

extern "C" {

/*
   C structure analogs to TraceVarInfo and TracePoint
   objects in TraceVarInfo.hpp and TracePoint.hpp respectively.

   The structures are required for interoperability with the LISP CFFI
   interface.
*/
typedef struct c_trace_var_info_struct {
    VarValue value;
    type_format format;
    uint32_t var_name_index;
    uint32_t type_name_index;
    uint32_t size;
    uint64_t buffer_size;
    uint8_t has_buffer_size;
} c_trace_var_info_struct;

typedef struct c_trace_point_struct {
    uint64_t statement;
    c_trace_var_info_struct *vars;
    uint32_t n_vars;
    uint64_t *aux;
    uint32_t n_aux;
} c_trace_point_struct;

/*
   Read trace from filename, returning NULL if no input is given by
   timeout_seconds.

   The returned trace pointer should be cleaned up with free_trace().

   Returns NULL if the trace can't be read or reading times out.
*/
void /* Trace */ *read_trace(const char *filename,
                             uint32_t timeout_seconds,
                             uint64_t max_points);

/*
   Return the number of points in the given trace.
*/
uint64_t trace_size(const void /* Trace */ *trace_ptr);

/*
   Return an array of trace points for the given trace.

   Note: The caller must deallocate this array.
*/
c_trace_point_struct* get_points(const void /* Trace */ *trace_ptr);

/*
   Return the number of types in the given trace.
*/
uint32_t n_types(const void /* Trace */ *trace_ptr);

/*
   Return an array of type names for the given trace.

   Note: The caller must deallocate this array.
*/
const char** get_types(const void /* Trace */ *trace_ptr);

/*
   Return the number of names in the given trace.
*/
uint32_t n_names(const void /* Trace */ *trace_ptr);

/*
   Return an array of names in the given trace.

   Note: The caller must deallocate this array.
*/
const char** get_names(const void /* Trace */ *trace_ptr);

/*
   Free the given trace.
*/
void free_trace(void /*Trace*/ *trace_ptr);

/*
   Free the given list of trace points.
*/
void free_trace_points(c_trace_point_struct* points, uint64_t n_points);

} // end extern "C"

/*
   Convert the given vector of TracePoint objects to a dynamically
   allocated array of c_trace_point_struct structs required for the LISP
   CFFI interface.
*/
c_trace_point_struct* points_to_array(const Trace & trace,
                                      const TracePoints & points);

#endif // __TRACE_H
