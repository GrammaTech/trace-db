#include <iostream>
#include <exception>

#include "Utils.hpp"
#include "trace.h"

void /* Trace */* read_trace(const char *filename,
                             uint32_t timeout_seconds,
                             uint64_t max)
{
    std::istream *in = 0;
    try {
        in = openWithTimeout(filename, timeout_seconds);
        Trace *trace = new Trace(*in, max);
        delete in;
        return trace;
    }
    catch (std::exception &ex) {
        delete in;
        return NULL;
    }
}

uint64_t trace_size(const void /* Trace */ *trace_ptr)
{
    const Trace *trace = (const Trace*) trace_ptr;
    return trace->getPoints().size();
}

c_trace_point_struct* get_points(const void /* Trace */ *trace_ptr)
{
    const Trace *trace = (const Trace*) trace_ptr;
    return points_to_array(*trace, trace->getPoints());
}

uint32_t n_types(const void /* Trace */ *trace_ptr)
{
    const Trace *trace = (const Trace*) trace_ptr;
    return trace->getTypes().size();
}

const char** get_types(const void /* Trace */ *trace_ptr)
{
    const Trace *trace = (const Trace*) trace_ptr;
    const TypeDescriptions & types = trace->getTypes();
    const char **ret = (const char**) calloc(types.size(), sizeof(char*));

    for (uint32_t i = 0; i < types.size(); i++) {
        ret[i] = trace->getNames()[types[i].getNameIndex()].c_str();
    }

    return ret;
}

uint32_t n_names(const void /* Trace */ *trace_ptr)
{
    const Trace *trace = (const Trace*) trace_ptr;
    return trace->getNames().size();
}

const char** get_names(const void /* Trace */ *trace_ptr)
{
    const Trace *trace = (const Trace*) trace_ptr;
    const Names & names = trace->getNames();
    const char **ret = (const char**) calloc(names.size(), sizeof(char*));

    for (uint32_t i = 0; i < names.size(); i++) {
        ret[i] = trace->getNames()[i].c_str();
    }

    return ret;
}

void free_trace(void /* Trace */ *trace_ptr)
{
    delete (Trace*) trace_ptr;
}

void free_trace_points(c_trace_point_struct* points, uint64_t n_points)
{
    for (uint64_t i = 0; i < n_points; i++) {
        free(points[i].vars);
        free(points[i].aux);
        points[i].vars = NULL;
        points[i].aux = NULL;
    }
    free(points);
    points = NULL;
}

c_trace_point_struct* points_to_array(const Trace & trace,
                                      const TracePoints & points)
{
    // Allocate buffer and return a copy of the results
    size_t results_size = points.size() * sizeof(c_trace_var_info_struct);
    c_trace_point_struct* results = NULL;

    if (results_size > 0) {
        results = (c_trace_point_struct*) malloc(results_size);

        size_t i = 0;
        for (auto & tracePoint : points) {
            FlyweightTraceVarInfos vars(tracePoint.getVars());
            std::vector<uint64_t> aux(tracePoint.getAux());
            size_t n_vars = vars.size();
            size_t n_aux = aux.size();
            c_trace_var_info_struct *c_vars = (c_trace_var_info_struct*)
                calloc(n_vars, sizeof(c_trace_var_info_struct));
            uint64_t *c_aux = (uint64_t*)
                calloc(n_aux, sizeof(uint64_t));

            results[i].statement = tracePoint.getStatement();
            results[i].n_aux     = n_aux;
            results[i].n_vars    = n_vars;
            results[i].aux       = c_aux;
            results[i].vars      = c_vars;

            std::copy(aux.begin(), aux.end(), c_aux);

            for (size_t j = 0; j < n_vars; j++) {
                const FlyweightTraceVarInfo var = vars[j];
                const TypeDescription & type = trace.getTypes()
                                                     [var.get().getTypeIndex()];

                c_vars[j].var_name_index = var.get().getNameIndex();
                c_vars[j].type_name_index = type.getNameIndex();
                c_vars[j].value = var.get().getValue();
                c_vars[j].format = var.get().getTypeFormat();
                c_vars[j].size = var.get().getSize();
                c_vars[j].buffer_size = var.get().getBufferSize();
                c_vars[j].has_buffer_size = var.get().hasBufferSize();
            }

            i++;
        }
    }

    return results;
}
