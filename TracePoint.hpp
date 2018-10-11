//
// TracePoint.hpp -- Immutable object representing a single instrumented
// trace point.  TracePoints are split into two classes; TracePointData
// and TracePoint.  TracePointData is a flyweight containing information
// which may be shared across multiple trace points, such as variables,
// buffer sizes, and aux data.  A TracePoint contains a flyweight
// TracePointData member along with members for the statement id
// and trace point counter, which are unique for each in the point.
//

#ifndef __TRACE_POINT_HPP
#define __TRACE_POINT_HPP

#include <cstdint>
#include <functional>
#include <istream>
#include <ostream>
#include <vector>
#include <unordered_map>
#include <utility>

#include <boost/archive/basic_archive.hpp>
#include <boost/flyweight.hpp>
#include <boost/flyweight/serialize.hpp>
#include <boost/functional/hash.hpp>
#include <boost/serialization/access.hpp>
#include <boost/serialization/split_member.hpp>

#include "QueryObjects.hpp"
#include "TraceBufferSize.hpp"
#include "TraceVarInfo.hpp"
#include "Utils.hpp"

class TracePointData;
class TracePoint;

typedef boost::flyweights::flyweight<TracePointData>
        FlyweightTracePointData;
typedef std::vector<uint64_t>
        Aux;
typedef std::vector<TracePoint>
        TracePoints;

enum trace_entry_tag {
    END_ENTRY = 0,
    STATEMENT_ID,
    VARIABLE,
    BUFFER_SIZE,
    /* A 64-bit value, meaning defined by the user */
    AUXILIARY,
    INVALID_TAG,
};

class TracePointData
{
private:
    friend class boost::serialization::access;

    TraceBufferSizes m_sizes;
    FlyweightTraceVarInfos m_vars;
    Aux m_aux;

    const FlyweightTraceVarInfo &
    varLookup(const std::vector<uint32_t> & bindings,
              uint64_t var_index) const {
        return m_vars[bindings[var_index]];
    }

    bool
    cachedSatisfiesPredicate(const Predicate & predicate,
                             const FlyweightTraceVarInfos & variables) const {
        static std::unordered_map<EvaluatePredicate, bool> m_cache;
        EvaluatePredicate functor(predicate, variables);

        if (m_cache.find(functor) == m_cache.end()) {
            m_cache[functor] = functor();
        }

        return m_cache[functor];
    }

public:
    TracePointData() :
        m_sizes(0),
        m_vars(0),
        m_aux(0)
    {}

    TracePointData(const TraceBufferSizes & sizes,
                   const FlyweightTraceVarInfos & vars,
                   const Aux & aux) :
        m_sizes(sizes),
        m_vars(vars),
        m_aux(aux)
    {}

    TracePointData(const TracePointData & other) :
        m_sizes(other.m_sizes),
        m_vars(other.m_vars),
        m_aux(other.m_aux)
    {}

    TracePointData& operator=(const TracePointData & other) = delete;

    inline const TraceBufferSizes & getBufferSizes() const {
        return m_sizes;
    }

    inline const FlyweightTraceVarInfos & getVars() const {
        return m_vars;
    }

    inline const Aux & getAux() const {
        return m_aux;
    }

    bool satisfiesPredicate(const Predicate & predicate,
                            const std::vector<uint32_t> & bindings) const {
        FlyweightTraceVarInfos variables;
        for (size_t i = 0; i < bindings.size(); i++) {
            variables.push_back(varLookup(bindings, i));
        }
        variables.shrink_to_fit();

        return cachedSatisfiesPredicate(predicate, variables);
    }

    inline bool operator<(const TracePointData & other) const {
        return std::tie(m_sizes,
                        m_vars,
                        m_aux) <
               std::tie(other.m_sizes,
                        other.m_vars,
                        other.m_aux);
    }

    inline bool operator==(const TracePointData & other) const {
        return m_sizes == other.m_sizes &&
               m_vars == other.m_vars &&
               m_aux == m_aux;
    }

    friend std::size_t hash_value(const TracePointData & tracePoint) {
        std::size_t seed = 0;

        boost::hash_combine(seed, tracePoint.m_sizes);
        boost::hash_combine(seed, tracePoint.m_vars);
        boost::hash_combine(seed, tracePoint.m_aux);

        return seed;
    }

    template<typename Archive>
    void load(Archive & ar,
              const unsigned int version) {
        size_t n_sizes;
        size_t n_vars;
        size_t n_aux;

        ar & n_sizes;
        ar & n_vars;
        ar & n_aux;

        m_sizes.reserve(n_sizes);
        m_vars.reserve(n_vars);
        m_aux.reserve(n_aux);

        for (size_t i = 0; i < n_sizes; i++) {
            TraceBufferSize size;
            ar & size;
            m_sizes.push_back(TraceBufferSize(size));
        }

        for (size_t i = 0; i < n_vars; i++) {
            TraceVarInfo var;
            ar & var;
            m_vars.push_back(FlyweightTraceVarInfo(var));
        }

        for (size_t i = 0; i < n_aux; i++) {
            uint64_t aux;
            ar & aux;
            m_aux.push_back(aux);
        }

        m_sizes.shrink_to_fit();
        m_vars.shrink_to_fit();
        m_aux.shrink_to_fit();
    }

    template<typename Archive>
    void save(Archive & ar,
              const unsigned int version) const {
        ar & m_sizes.size();
        ar & m_vars.size();
        ar & m_aux.size();

        for (auto & size : m_sizes) {
            ar & size;
        }

        for (auto & var : m_vars) {
            ar & var.get();
        }

        for (auto & aux : m_aux) {
            ar & aux;
        }
    }

    BOOST_SERIALIZATION_SPLIT_MEMBER();
};

namespace std {
    template <>
    struct hash<TracePointData>
    {
        std::size_t
        operator()(const TracePointData & tracePointData) const {
            return hash_value(tracePointData);
        }
    };
}

class TracePoint
{
private:
    friend class boost::serialization::access;

    uint64_t m_counter;
    uint64_t m_statement;
    FlyweightTracePointData m_data;

    std::istream & read(std::istream & is,
                        const Names & names,
                        const TypeDescriptions & types) {
        MemoryMap map;
        return read(is, 0, names, types, map);
    }

    std::istream & read(std::istream & is,
                        const Names & names,
                        const TypeDescriptions & types,
                        MemoryMap & map) {
        return read(is, 0, names, types, map);
    }

    std::istream & read(std::istream & is,
                        const uint64_t counter,
                        const Names & names,
                        const TypeDescriptions & types,
                        MemoryMap & map) {
        TraceBufferSizes sizes(0);
        FlyweightTraceVarInfos vars(0);
        Aux aux;

        m_counter = counter;

        while (1) {
            char c = (char) INVALID_TAG;
            BINARY_READ(is, &c, sizeof(c));
            enum trace_entry_tag tag = (trace_entry_tag) c;

            if (!is.good() || tag == END_ENTRY) {
                break;
            } else if (c >= INVALID_TAG) {
                throw TraceError("Invalid trace point tag.");
            }

            switch (tag) {
            case STATEMENT_ID:
                BINARY_READ(is, &m_statement, sizeof(m_statement));
                break;
            case BUFFER_SIZE:
                {
                    TraceBufferSize size(is);
                    map.updateMemoryMap(size);
                    sizes.push_back(size);
                    break;
                }
            case VARIABLE:
                {
                    TraceVarInfo info(is, names, types, map);
                    vars.push_back(FlyweightTraceVarInfo(info));
                    break;
                }

            case AUXILIARY:
                {
                    static uint64_t value;
                    BINARY_READ(is, &value, sizeof(value));
                    aux.push_back(value);
                    break;
                }
            default:
                throw TraceError("Invalid trace point tag.");
            }
        }

        sizes.shrink_to_fit();
        vars.shrink_to_fit();
        aux.shrink_to_fit();

        m_data = FlyweightTracePointData(sizes, vars, aux);

        return is;
    }

    std::ostream & write(std::ostream & os) const {
        os.put(STATEMENT_ID);
        BINARY_WRITE(os, &m_statement, sizeof(m_statement));

        for (auto & bufferSize : getBufferSizes()) {
            os.put(BUFFER_SIZE);
            os << bufferSize;
        }

        for (auto & var : getVars()) {
            os.put(VARIABLE);
            os << var;
        }

        for (auto & aux : getAux()) {
            os.put(AUXILIARY);
            BINARY_WRITE(os, &aux, sizeof(aux));
        }

        os.put(END_ENTRY);
        return os;
    }

public:
    TracePoint() :
        m_counter(0),
        m_statement(0),
        m_data()
    {}

    TracePoint(uint64_t counter,
               uint64_t statement,
               const TraceBufferSizes & sizes,
               const FlyweightTraceVarInfos & vars,
               const Aux & aux) :
        m_counter(counter),
        m_statement(statement),
        m_data(sizes, vars, aux)
    {}

    TracePoint(uint64_t counter,
               uint64_t statement,
               const TracePointData & data) :
        m_counter(counter),
        m_statement(statement),
        m_data(data)
    {}

    TracePoint(const TracePoint & other) :
        m_counter(other.m_counter),
        m_statement(other.m_statement),
        m_data(other.m_data)
    {}

    TracePoint(std::istream & is,
               const Names & names,
               const TypeDescriptions & types) {
        read(is, names, types);
    }

    TracePoint(std::istream & is,
               const Names & names,
               const TypeDescriptions & types,
               MemoryMap & map) {
        read(is, names, types, map);
    }

    TracePoint(std::istream & is,
               const uint64_t counter,
               const Names & names,
               const TypeDescriptions & types,
               MemoryMap & map) {
        read(is, counter, names, types, map);
    }

    TracePoint& operator=(const TracePoint & other) = delete;

    inline uint64_t getCounter() const {
        return m_counter;
    }

    inline uint64_t getStatement() const {
        return m_statement;
    }

    inline FlyweightTracePointData getData() const {
        return m_data;
    }

    inline const TraceBufferSizes & getBufferSizes() const {
        return m_data.get().getBufferSizes();
    }

    inline const FlyweightTraceVarInfos & getVars() const {
        return m_data.get().getVars();
    }

    inline const std::vector<uint64_t> & getAux() const {
        return m_data.get().getAux();
    }

    bool satisfiesPredicate(const Predicate & predicate,
                            const std::vector<uint32_t> & bindings) const {
        return m_data.get().satisfiesPredicate(predicate, bindings);
    }

    inline bool operator<(const TracePoint & other) const {
        return std::tie(m_counter,
                        m_statement,
                        m_data) <
               std::tie(other.m_counter,
                        other.m_statement,
                        other.m_data);
    }

    inline bool operator==(const TracePoint & other) const {
        return m_counter == other.m_counter &&
               m_statement == other.m_statement &&
               m_data == m_data;
    }

    friend std::size_t hash_value(const TracePoint & tracePoint) {
        std::size_t seed = 0;

        boost::hash_combine(seed, tracePoint.m_counter);
        boost::hash_combine(seed, tracePoint.m_statement);
        boost::hash_combine(seed, tracePoint.m_data);

        return seed;
    }

    friend std::ostream & operator<<(std::ostream & os,
                                     const TracePoint & tracePoint) {
        return tracePoint.write(os);
    }

    template<typename Archive>
    void serialize(Archive & ar,
                   const unsigned int version) {
        ar & m_counter;
        ar & m_statement;
        ar & m_data;
    }
};

namespace std {
    template <>
    struct hash<TracePoint>
    {
        std::size_t
        operator()(const TracePoint & tracePoint) const {
            return hash_value(tracePoint);
        }
    };
}

#endif // __TRACE_POINT_HPP
