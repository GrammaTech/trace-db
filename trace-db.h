#ifndef __TRACE_DB_H
#define __TRACE_DB_H

#ifdef __cplusplus
extern "C" {
#endif
#include <cassert>

#include <boost/archive/basic_archive.hpp>

#include "read-trace.h"

typedef struct trace
{
    trace_point *points;
    uint64_t n_points;
    uint64_t n_points_allocated;

    char **names;
    uint32_t n_names;
    type_description *types;
    uint32_t n_types;
} trace;

typedef struct trace_db
{
    trace *traces;
    uint64_t n_traces;
    uint64_t n_traces_allocated;
} trace_db;

/* Allocate a trace DB */
trace_db *create_db();
/* Read a trace into the DB */
void add_trace(trace_db *db, trace_read_state *state, uint64_t max);
/* Store an existing trace in the DB */
void set_trace(trace_db *db, uint32_t index, trace *trace);
/* Serialize a trace DB */
char* serialize_trace_db(trace_db *db);
/* Deserialize a trace DB */
trace_db* deserialize_trace_db(const char* text);
/* Deallocate a trace DB */
void free_db(trace_db *db);

typedef struct free_variable
{
    uint32_t n_allowed_types;
    /* Indices of compatible types */
    uint32_t *allowed_types;
} free_variable;

/* QUERY PREDICATES

   A predicate is a simple expression tree containing logical and arithmetic
   operators, numeric comparisons, variables, and numeric values.

   Predicates are type-checked during query processing, and will generate
   assertion failures if, for example, a number is used where a boolean value
   is required.
*/
enum predicate_kind
{
    /* Properties of variables */
    VAR_REFERENCE,
    VAR_SIZE,
    VAR_VALUE,
    DISTINCT_VARS,
    /* Numeric values */
    SIGNED_INTEGER,
    UNSIGNED_INTEGER,
    /* Logical operators */
    AND,
    OR,
    NOT,
    /* Comparisons */
    GREATER_THAN,
    LESS_THAN,
    EQUAL,
    /* Arithmetic operators */
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE
};

typedef struct predicate
{
    enum predicate_kind kind;
    union {
        uint64_t n_children;
        uint64_t var_index;
        uint64_t unsigned_value;
        int64_t signed_value;
    } data;
    struct predicate *children;
} predicate;

/* Return all results which match query.

   Each result is represented as a trace point containing only the statement
   ID and bound variables.

   If pick is true, randomly choose a single trace point and return results
   from that point.

   *n_results_out will be set to the number of results. If the number of
   *results is non-zero, then *results_out will be set to an array of
   *results.

   The caller must pass the results to free_query_result to deallocate
   memory.

   Results are generated at each trace point by finding all possible bindings
   for the free variables, then evaluating the predicate over those variables
   to filter. If there are no free variables, each trace point will generate
   one result.

   Trace points where (statement_id & statement_mask) != statement are
   skipped. Setting both arguments to zero will ensure that all statements
   are queried.
*/
void query_trace(const trace_db *db, uint64_t index,
                 uint32_t n_variables, const free_variable *variables,
                 const predicate *hard_predicate,
                 const predicate **soft_predicates, uint32_t n_soft_predicates,
                 uint32_t seed, uint64_t statement_mask, uint64_t statement,
                 trace_point **results_out, uint64_t *n_results_out);
void free_query_result(trace_point *results, uint64_t n_results);
void free_predicate(predicate *predicate);

typedef struct skip_list skip_list;

/* Create a skip list for tracking memory regions */
skip_list *create_memory_map();
void free_memory_map(skip_list *list);
/* Update memory map with buffer sizes from point.  */
void update_memory_map(skip_list *memory_map, const trace_point *point);
/* Compute size of buffer from memory map, storing the result into a
   var_info. */
void compute_buffer_size(const skip_list *memory_map,
                         const trace_read_state *state,
                         trace_var_info *var);

#ifdef __cplusplus
} // end extern "C"
#endif

bool operator==(const trace & a, const trace & b);
bool operator==(const trace_db & a, const trace_db & b);

namespace boost {
namespace serialization {

template<class Archive>
void save(Archive & ar,
          const trace & trace,
          const unsigned int version) {
    ar & trace.n_points;
    ar & trace.n_points_allocated;
    ar & trace.n_names;
    ar & trace.n_types;

    for (uint64_t i = 0; i < trace.n_points; i++)
        ar & trace.points[i];

    for (uint32_t i = 0; i < trace.n_names; i++) {
        size_t len = strlen(trace.names[i]) + 1;
        ar & len;
        for (size_t j = 0; j < len; j++) {
            ar & trace.names[i][j];
        }
    }

    for (uint32_t i = 0; i< trace.n_types; i++)
        ar & trace.types[i];
}

template<class Archive>
void load(Archive & ar,
          trace & trace,
          const unsigned int version) {
    ar & trace.n_points;
    ar & trace.n_points_allocated;
    ar & trace.n_names;
    ar & trace.n_types;

    assert(trace.n_points >= 0);
    assert(trace.n_points_allocated >= 0);
    assert(trace.n_names >= 0);
    assert(trace.n_types >= 0);

    trace.points = NULL;
    trace.names = NULL;
    trace.types = NULL;

    if (trace.n_points_allocated > 0)
        trace.points = (trace_point*) calloc(trace.n_points_allocated,
                                             sizeof(trace_point));
    if (trace.n_names > 0)
        trace.names = (char **) calloc(trace.n_names,
                                       sizeof(char*));
    if (trace.n_types > 0)
        trace.types = (type_description *) calloc(trace.n_types,
                                                  sizeof(type_description));

    for (uint64_t i = 0; i < trace.n_points; i++)
        ar & trace.points[i];

    for (uint32_t i = 0; i < trace.n_names; i++) {
        size_t len;
        ar & len;
        trace.names[i] = (char*) calloc(len, sizeof(char));
        for (size_t j = 0; j < len; j++) {
            ar & trace.names[i][j];
        }
    }

    for (uint32_t i = 0; i < trace.n_types; i++)
        ar & trace.types[i];
}

template<class Archive>
void serialize(Archive & ar,
               trace & trace,
               const unsigned int file_version) {
    split_free(ar, trace, file_version);
}

template<class Archive>
void save(Archive & ar,
          const trace_db & trace_db,
          const unsigned int version) {
    ar & trace_db.n_traces;
    ar & trace_db.n_traces_allocated;

    for (uint64_t i = 0; i < trace_db.n_traces; i++)
        ar & trace_db.traces[i];
}

template<class Archive>
void load(Archive & ar,
          trace_db & trace_db,
          const unsigned int version) {
    ar & trace_db.n_traces;
    ar & trace_db.n_traces_allocated;

    assert(trace_db.n_traces >= 0);
    assert(trace_db.n_traces_allocated >= 0);

    trace_db.traces = NULL;
    if (trace_db.n_traces_allocated > 0)
        trace_db.traces = (trace*) calloc(trace_db.n_traces, sizeof(trace));

    for (uint64_t i = 0; i < trace_db.n_traces; i++)
        ar & trace_db.traces[i];
}

template<class Archive>
void serialize(Archive & ar,
               trace_db & trace_db,
               const unsigned int file_version) {
    split_free(ar, trace_db, file_version);
}

} // end namespace serialization
} // end namespace boost

#endif // __TRACE_DB_H
