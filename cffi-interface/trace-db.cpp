#include <algorithm>
#include <cassert>
#include <cstring>
#include <functional>
#include <vector>
#include <unordered_set>
#include <sstream>

#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>

#include "QueryObjects.hpp"
#include "TypeDescription.hpp"
#include "TraceVarInfo.hpp"
#include "TracePoint.hpp"
#include "Trace.hpp"
#include "TraceDB.hpp"
#include "Utils.hpp"

#include "trace.h"
#include "trace-db.h"

void /* TraceDB */ *create_db()
{
    return new TraceDB();
}

int add_trace(void /* TraceDB */ *db_ptr,
              const char *filename,
              uint32_t timeout_seconds,
              uint64_t max_points)
{
    try {
        TraceDB *db = (TraceDB *) db_ptr;
        std::istream *in = openWithTimeout(filename, timeout_seconds);
        Trace trace(*in, max_points);
        db->addTrace(trace);
        delete in;
        return true;
    }
    catch (std::exception &ex) {
        return false;
    }
}

int add_trace_points(void /* TraceDB */ *db_ptr,
                     c_trace_point_struct* c_points,
                     uint64_t n_points,
                     char** c_names,
                     uint32_t n_names)
{
    /*
      IMPORTANT: This method is for legacy testing purposes only.
      This method is not complete or efficient.
      DO NOT USE IN PRODUCTION OR NEW DEVELOPMENT.
    */
    TraceDB *db = (TraceDB*) db_ptr;
    Names names;
    TypeDescriptions types;
    TracePoints points;
    std::map<uint32_t, type_format> type_formats;

    for (uint64_t i = 0; i < n_names; i++) {
        names.push_back(std::string(c_names[i]));
    }


    for (uint64_t i = 0; i < n_points; i++) {
        for (uint32_t j = 0; j < c_points[i].n_vars; j++) {
            if (c_points[i].vars[j].value.s < 0) {
                type_formats[c_points[i].vars[j].type_name_index] = SIGNED;
            } else if (type_formats.find(c_points[i].vars[j]
                                                    .type_name_index) ==
                       type_formats.end()) {
                type_formats[c_points[i].vars[j]
                                        .type_name_index] = UNSIGNED;
            }
        }
    }

    for (auto type_format : type_formats) {
        types.push_back(TypeDescription(type_format.first,
                                        type_format.second,
                                        sizeof(uint64_t)));
    }

    for (uint64_t i = 0; i < n_points; i++) {
        FlyweightTraceVarInfos vars;
        Aux aux;

        for (uint32_t j = 0; j < c_points[i].n_aux; j++) {
            aux[j] = c_points[i].aux[j];
        }

        for (uint32_t j = 0; j < c_points[i].n_vars; j++) {
            uint32_t type_index =
                std::distance(types.begin(),
                              std::find_if(
                                  types.begin(),
                                  types.end(),
                                  [c_points, i, j](const TypeDescription & a)
                                      -> bool {
                                      return a.getNameIndex() ==
                                             c_points[i].vars[j]
                                                        .type_name_index;
                                  }));

            vars.push_back(
                FlyweightTraceVarInfo(c_points[i].vars[j].value,
                                      c_points[i].vars[j].var_name_index,
                                      type_index,
                                      types[type_index].getTypeFormat(),
                                      types[type_index].getSize(),
                                      c_points[i].vars[j].buffer_size,
                                      c_points[i].vars[j].has_buffer_size));

        }

        vars.shrink_to_fit();
        aux.shrink_to_fit();

        points.push_back(TracePoint(i,
                                    c_points[i].statement,
                                    TraceBufferSizes(0),
                                    vars,
                                    aux));
    }

    names.shrink_to_fit();
    types.shrink_to_fit();
    points.shrink_to_fit();

    db->addTrace(Trace(names, types, points));

    return true;
}

const void /* Trace */ *get_trace(const void /* TraceDB */ *db_ptr,
                                  uint32_t index)
{
    const TraceDB *db = (const TraceDB*) db_ptr;
    return &(db->getTraces()[index]);
}

uint32_t n_traces(const void /* TraceDB */ *db_ptr)
{
    const TraceDB *db = (const TraceDB*) db_ptr;
    return db->getTraces().size();
}

char* serialize_trace_db(const void /* TraceDB */ *db_ptr)
{
    std::stringstream ss;
    boost::archive::text_oarchive oa(ss);
    const TraceDB *db = (const TraceDB*) db_ptr;
    oa << *db;

    std::string t = ss.str();
    char * ret = NULL;
    ret = (char*) malloc(t.length() + 1);
    memcpy(ret, t.c_str(), t.length() + 1);
    return ret;
}

void /* TraceDB */ * deserialize_trace_db(const char* text)
{
    TraceDB * db = (TraceDB*) create_db();

    std::stringstream ss;
    ss << std::string(text);
    boost::archive::text_iarchive ia(ss);
    ia >> *db;

    return db;
}

void free_db(void /* TraceDB */ *db_ptr)
{
    TraceDB *db = (TraceDB *) db_ptr;
    delete db;
}

static FreeVariable
create_free_variable_obj(const c_free_variable c_free_variable)
{
    return FreeVariable(std::vector<uint32_t>(c_free_variable.allowed_types,
                                              c_free_variable.allowed_types +
                                              c_free_variable.n_allowed_types));
}

static Predicate
create_predicate_obj(const c_predicate *c_predicate)
{
    if (c_predicate == NULL) {
        return Predicate();
    }
    else {
        Predicates children;
        union predicate_data data;

        if (c_predicate->children) {
            for (uint32_t i = 0; i < c_predicate->n_children; i++) {
                Predicate child(create_predicate_obj(c_predicate->children+i));

                if (child.getKind() != NULL_PREDICATE) {
                    children.push_back(child);
                }
            }
        }
        children.shrink_to_fit();

        switch (c_predicate->kind) {
        case VAR_REFERENCE:
            data.var_index = c_predicate->var_index;
            break;
        case UNSIGNED_VALUE:
            data.unsigned_value = c_predicate->unsigned_value;
            break;
        case SIGNED_VALUE:
            data.signed_value = c_predicate->signed_value;
            break;
        case FLOAT_VALUE:
            data.float_value = c_predicate->float_value;
            break;
        default:
            data.n_children = c_predicate->n_children;
            break;
        }

        return Predicate(c_predicate->kind,
                         data,
                         children);
    }
}

static void free_c_predicate_helper(c_predicate *c_predicate)
{
    if (!(c_predicate->kind == VAR_REFERENCE ||
          c_predicate->kind == SIGNED_VALUE ||
          c_predicate->kind == UNSIGNED_VALUE ||
          c_predicate->kind == FLOAT_VALUE)) {
        for (uint32_t i = 0; i < c_predicate->n_children; i++)
            free_c_predicate_helper(c_predicate->children + i);
        free(c_predicate->children);
    }
}

void free_c_predicate(c_predicate *c_predicate)
{
    if (c_predicate != NULL) {
        free_c_predicate_helper(c_predicate);
        free(c_predicate);
    }
}

TracePoints deduplicate_results(const TracePoints & results) {
    auto hashfn = [](const TracePoint & point) {
        std::size_t seed = 0;

        boost::hash_combine(seed, point.getStatement());
        for (auto & var : point.getVars()) {
            boost::hash_combine(seed, var.get().getNameIndex());
            boost::hash_combine(seed, var.get().getTypeIndex());
        }

        return seed;
    };
    auto equalfn = [](const TracePoint & lhs, const TracePoint & rhs) {
        bool ret = true;
        ret &= lhs.getStatement() == rhs.getStatement();
        ret &= lhs.getVars().size() == rhs.getVars().size();

        for (uint32_t i = 0; (ret && i < lhs.getVars().size()); i++) {
            ret &= lhs.getVars()[i].get().getNameIndex() ==
                   rhs.getVars()[i].get().getNameIndex();
            ret &= lhs.getVars()[i].get().getTypeIndex() ==
                   rhs.getVars()[i].get().getTypeIndex();
        }

        return ret;
    };

    std::unordered_set<TracePoint, decltype(hashfn), decltype(equalfn)>
        set(results.size() / 2, hashfn, equalfn);
    TracePoints deduplicated_results;

    for (auto & tracePoint : results) {
        if (set.find(tracePoint) == set.end()) {
            deduplicated_results.push_back(tracePoint);
        }
        set.insert(tracePoint);
    }

    return deduplicated_results;
}

void query_trace(const void /* TraceDB */ *db_ptr, uint64_t index,
                 const c_free_variable *c_free_variables, uint32_t n_variables,
                 const c_predicate *c_hard_predicate,
                 const c_predicate **c_soft_predicates, uint32_t n_soft_predicates,
                 uint32_t seed, uint8_t keep_duplicate_bindings,
                 uint64_t statement_mask, uint64_t statement,
                 c_trace_point_struct **c_results_out, uint64_t *n_results_out)
{
    const TraceDB *db = (const TraceDB *) db_ptr;
    assert(index < db->getTraces().size());

    // Retrieve the trace
    const Trace & trace = db->getTraces()[index];

    // Convert C structs to C++ objects for query
    FreeVariables variables;
    Predicates soft_predicates;
    Predicate hard_predicate(create_predicate_obj(c_hard_predicate));

    for (uint32_t i = 0; i < n_variables; i++) {
        variables.push_back(create_free_variable_obj(c_free_variables[i]));
    }

    for (uint32_t i = 0; i < n_soft_predicates; i++) {
        soft_predicates.push_back(create_predicate_obj(c_soft_predicates[i]));
    }

    // Query the trace
    TracePoints results = trace.query(variables,
                                      hard_predicate,
                                      soft_predicates,
                                      seed,
                                      statement_mask,
                                      statement);

    // Filter duplicates from consideration
    if (!keep_duplicate_bindings) {

        results = deduplicate_results(results);
    }

    // Convert back to C and return results
    *n_results_out = results.size();
    *c_results_out = points_to_array(trace, results);
}
