//
// Trace.hpp -- Representation of a single trace in the trace database.
// Each trace consists of a vector of names for variables and types,
// a vector of type descriptions, and finally, a vector of trace points.
//

#ifndef __TRACE_HPP
#define __TRACE_HPP

#include <algorithm>
#include <cassert>
#include <istream>
#include <iterator>
#include <ostream>
#include <map>
#include <random>
#include <string>
#include <utility>
#include <vector>

#include <boost/archive/basic_archive.hpp>
#include <boost/flyweight.hpp>
#include <boost/functional/hash.hpp>
#include <boost/serialization/access.hpp>
#include <boost/serialization/split_member.hpp>

#include "QueryObjects.hpp"
#include "MemoryMap.hpp"
#include "TypeDescription.hpp"
#include "TracePoint.hpp"
#include "Utils.hpp"

typedef std::pair<uint64_t, uint64_t> MaskKey;

class Trace
{
private:
    friend class boost::serialization::access;

    Names m_names;
    TypeDescriptions m_types;
    TracePoints m_points;

    mutable MemoryMap m_memory_map;
    mutable std::map<MaskKey, TracePoints> m_getpoints_cache;

    std::istream & read(std::istream & is) {
        return read(is, 0);
    }

    std::istream & read(std::istream & is,
                        uint64_t max_points) {
        uint64_t n_chars = 0;
        uint32_t n_types = 0;
        uint64_t i = 0;
        char *buf = NULL;

        /* Read dictionary of names */
        BINARY_READ(is, &n_chars, sizeof(n_chars));
        if (n_chars > 0) {
            buf = new char[n_chars];
            BINARY_READ(is, buf, n_chars);

            do {
                m_names.push_back(std::string(buf + i));
                while (buf[i] != 0) {
                    i++;
                }
                i++;
            } while (i < n_chars);

            delete[] buf;
        }

        /* Read dictionary of types */
        BINARY_READ(is, &n_types, sizeof(n_types));
        m_types.reserve(n_types);

        for (i = 0; i < n_types; i++) {
            TypeDescription type(is);
            m_types.push_back(type);
        }

        /* Read points */
        for (i = 0; is.good() && (i < max_points || max_points == 0); i++) {
            TracePoint point(is, i, m_names, m_types, m_memory_map);

            if (is.good()) {
                m_points.push_back(point);
            }
        }

        m_names.shrink_to_fit();
        m_types.shrink_to_fit();
        m_points.shrink_to_fit();

        return is;
    }

    std::ostream & write(std::ostream & os) const {
        uint64_t n_chars = 0;
        uint32_t n_types = m_types.size();

        for (auto & name : m_names) {
            n_chars += name.length() + 1;
        }

        BINARY_WRITE(os, &n_chars, sizeof(n_chars));
        for (auto & name : m_names) {
            BINARY_WRITE(os, name.c_str(), name.length() + 1);
        }

        BINARY_WRITE(os, &n_types, sizeof(n_types));
        for (auto & type : m_types) {
            os << type;
        }

        for (auto & point : m_points) {
            os << point;
        }

        return os;
    }

    // Cartesian product of vectors
    static std::vector<std::vector<uint32_t> >
    cartesian(const std::vector<std::vector<uint32_t> > & vectors) {
        std::vector<std::vector<uint32_t> > results;
        std::vector<uint32_t> current(vectors.size());

        size_t n_results = 1;
        for (auto & v : vectors)
            n_results *= v.size();
        results.reserve(n_results);

        // Enumerate all results, using modular arithmetic to index into
        // individual vectors.
        for (size_t result_i = 0; result_i < n_results; result_i++) {
            size_t quotient = result_i;
            for (size_t v_i = 0; v_i < vectors.size(); v_i++) {
                auto & v = vectors[v_i];
                size_t n = quotient % v.size();
                current[v_i] = v[n];
                quotient /= v.size();
            }
            results.push_back(current);
        }

        return results;
    }

    static bool typeCompatible(const FreeVariable & free_var,
                               const TraceVarInfo & real_var) {
        for (auto & allowedType : free_var.getAllowedTypes()) {
            if (allowedType == real_var.getTypeIndex())
                return true;
        }
        return false;
    }

    TracePoints
    collectResultsAtPoint(const TracePoint & current,
                          const FreeVariables & variables,
                          const Predicate & hard_predicate,
                          const Predicates & soft_predicates) const
    {
        TracePoints hard_query_results(0);

        // No free variables. Match every trace point once, with no bindings.
        if (variables.empty()) {
            // can't have predicate without variables
            assert(hard_predicate.getKind() == NULL_PREDICATE);
            TracePoint point(current.getCounter(),
                             current.getStatement(),
                             TraceBufferSizes(),
                             FlyweightTraceVarInfos(),
                             std::vector<uint64_t>());
            hard_query_results.push_back(point);
        }

        // Find possible bindings for each free variable
        std::vector<std::vector<uint32_t> > matching_vars(variables.size());
        for (uint32_t free_i = 0; free_i < variables.size(); free_i++) {
            for (uint32_t var_i = 0; var_i < current.getVars().size(); var_i++) {
                if (typeCompatible(variables[free_i],
                                   current.getVars()[var_i]))
                    matching_vars[free_i].push_back(var_i);
            }
        }

        // Collect all combinations of bindings
        for (auto bindings : cartesian(matching_vars)) {
            if (current.satisfiesPredicate(hard_predicate, bindings)) {
                FlyweightTraceVarInfos vars;

                vars.reserve(bindings.size());
                for (uint32_t i = 0; i < bindings.size(); i++) {
                    vars.push_back(current.getVars()[bindings[i]]);
                }

                TracePoint point(current.getCounter(),
                                 current.getStatement(),
                                 current.getBufferSizes(),
                                 vars,
                                 current.getAux());
                hard_query_results.push_back(point);
            }
        }

        // Maximize soft_predicates
        if (!soft_predicates.empty()) {
            TracePoints soft_query_results(0);
            std::vector<unsigned int> sat_counts;

            // Variables were reordered above so effective bindings are the same
            // for all results.
            std::vector<uint32_t> bindings(variables.size());
            for (unsigned int i = 0; i < variables.size(); i++)
                bindings[i] = i;

            unsigned int max_sat = 0;
            for (auto & r : hard_query_results) {
                unsigned int this_count = 0;
                for (auto & soft_predicate : soft_predicates) {
                    if (r.satisfiesPredicate(soft_predicate,
                                             bindings))
                        this_count++;
                }
                sat_counts.push_back(this_count);
                max_sat = std::max(max_sat, this_count);
            }

            for (unsigned int i = 0; i < hard_query_results.size(); i++) {
                if (sat_counts[i] == max_sat)
                    soft_query_results.push_back(hard_query_results[i]);
            }

            return soft_query_results;
        }
        else {
            return hard_query_results;
        }
    }

public:
    Trace() :
        m_names(0),
        m_types(0),
        m_points(0),
        m_memory_map(),
        m_getpoints_cache()
    {}

    Trace(const Names & names,
          const TypeDescriptions & types,
          const TracePoints & points) :
        m_names(names),
        m_types(types),
        m_points(points),
        m_memory_map(),
        m_getpoints_cache()
    {}

    Trace(std::istream & is, uint64_t max_points = 0) {
        read(is, max_points);
    }

    Trace(const Trace & other) :
        m_names(other.m_names),
        m_types(other.m_types),
        m_points(other.m_points),
        m_memory_map(),
        m_getpoints_cache()
    {}

    Trace& operator=(const Trace & other) = delete;

    inline const TracePoints & getPoints() const {
        return m_points;
    }

    inline const TracePoints & getPoints(uint64_t mask,
                                         uint64_t statement) const {
        // Return all points where the statement id bitmasked with MASK
        // is equal to STATEMENT.
        const std::pair<uint64_t, uint64_t> key =
            std::make_pair(mask, statement);

        if (m_getpoints_cache.find(key) == m_getpoints_cache.end()) {
            m_getpoints_cache[key] = TracePoints();
            for (auto & point : m_points) {
                if ((point.getStatement() & mask) == statement) {
                    m_getpoints_cache[key].push_back(point);
                }
            }
        }

        return m_getpoints_cache[key];
    }

    inline const Names & getNames() const {
        return m_names;
    }

    inline const TypeDescriptions & getTypes() const {
        return m_types;
    }

    /*
       Return all results which satisfy the predicate.

       Results are generated at each trace point by finding all possible
       bindings for the free variables and then evaluating the hard predicate
       over those variables to filter.  If there are no free variables, each
       trace point will generate one result.  If soft predicates are given,
       we perform an additional round of filtering to return those results
       which maximize the number of soft predicates satisfied.

       If seed is non-zero, randomly choose a single trace point and return
       results from that point.

       Trace points where (statement_id & statement_mask) != statement are
       skipped.  Setting both arguments to zero will ensure all statements
       are queried.
    */
    TracePoints
    query(const FreeVariables & variables,
          const Predicate & hard_predicate,
          const Predicates & soft_predicates = Predicates(),
          uint32_t seed = 0,
          uint64_t statement_mask = 0, uint64_t statement = 0) const {
        TracePoints results;
        const TracePoints points = this->getPoints(statement_mask, statement);

        // Perform the query on the filtered points
        if (seed) {
            std::mt19937 mt(seed);
            std::uniform_int_distribution<uint64_t> dist(0, points.size()-1);

            if (!points.empty()) {
                results = collectResultsAtPoint(
                              points[dist(mt)],
                              variables,
                              hard_predicate,
                              soft_predicates);
            }
        }
        else {
            for (auto point : points) {
                std::vector<TracePoint> singlePointResults =
                    collectResultsAtPoint(point,
                                          variables,
                                          hard_predicate,
                                          soft_predicates);
                std::move(std::begin(singlePointResults),
                          std::end(singlePointResults),
                          std::back_inserter(results));
            }
        }

        return results;
    }

    inline bool operator==(const Trace & other) const {
        return m_points == other.m_points &&
               m_names == other.m_names &&
               m_types == other.m_types;
    }

    friend std::size_t hash_value(const Trace & trace) {
        std::size_t seed = 0;

        boost::hash_combine(seed, trace.m_points);
        boost::hash_combine(seed, trace.m_names);
        boost::hash_combine(seed, trace.m_types);

        return seed;
    }

    friend std::ostream & operator<<(std::ostream & os, const Trace & trace) {
        return trace.write(os);
    }

    template<typename Archive>
    void load(Archive & ar,
              const unsigned int version) {
        uint64_t n_points;
        uint32_t n_names;
        uint32_t n_types;

        ar & n_points;
        ar & n_names;
        ar & n_types;

        m_points.resize(n_points);
        m_names.resize(n_names);
        m_types.resize(n_types);

        for (size_t i = 0; i < n_points; i++) {
            ar & m_points[i];
        }

        for (size_t i = 0; i < n_names; i++) {
            ar & m_names[i];
        }

        for (size_t i = 0; i < n_types; i++) {
            ar & m_types[i];
        }

        m_points.shrink_to_fit();
        m_names.shrink_to_fit();
        m_types.shrink_to_fit();
    }

    template<typename Archive>
    void save(Archive & ar,
              const unsigned int version) const {
        uint64_t n_points = m_points.size();
        uint32_t n_names = m_names.size();
        uint32_t n_types = m_types.size();

        ar & n_points;
        ar & n_names;
        ar & n_types;

        for (auto & point : m_points) {
            ar & point;
        }

        for (auto & name : m_names) {
            ar & name;
        }

        for (auto & type : m_types) {
            ar & type;
        }
    }

    BOOST_SERIALIZATION_SPLIT_MEMBER();
};

namespace std {
    template <>
    struct hash<Trace>
    {
        std::size_t
        operator()(const Trace & trace) const {
            return hash_value(trace);
        }
    };
}

#endif // __TRACE_HPP
