//
// TraceDB.hpp -- Immutable object representing a collection of traces.
// The class is mainly a thin wrapper around a vector of traces.
//

#ifndef __TRACE_DB_HPP
#define __TRACE_DB_HPP

#include <cstdint>
#include <mutex>
#include <vector>

#include <boost/archive/basic_archive.hpp>
#include <boost/functional/hash.hpp>
#include <boost/serialization/access.hpp>

#include "Trace.hpp"

typedef std::vector<Trace> Traces;

class TraceDB
{
private:
    friend class boost::serialization::access;

    Traces m_traces;
    std::mutex m_mutex;
public:
    TraceDB() :
        m_traces(),
        m_mutex()
    {}

    TraceDB(const std::vector<Trace> & traces) :
        m_traces(traces),
        m_mutex()
    {}

    TraceDB(const TraceDB & other) :
        m_traces(other.m_traces),
        m_mutex()
    {}

    TraceDB& operator=(const TraceDB & other) = delete;

    void addTrace(const Trace & trace) {
        // mutex allows multiple traces to be added at once,
        // as required for test case threading in the software
        // evolution library
        std::lock_guard<std::mutex> lock(m_mutex);
        m_traces.push_back(trace);
    }

    inline const Traces & getTraces() const {
        return m_traces;
    }

    inline bool operator==(const TraceDB & other) const {
        return m_traces == other.m_traces;
    }

    TracePoints
    query(uint32_t index,
          const std::vector<FreeVariable> & variables,
          const Predicate & hard_predicate,
          const Predicates & soft_predicates = Predicates(),
          uint32_t seed = 0,
          uint64_t statement_mask = 0, uint64_t statement = 0) const {
        assert(index < m_traces.size());
        return m_traces[index].query(variables,
                                     hard_predicate,
                                     soft_predicates,
                                     seed,
                                     statement_mask,
                                     statement);
    }

    friend std::size_t hash_value(const TraceDB & traceDB) {
        boost::hash<std::vector<Trace>> hasher;
        return hasher(traceDB.m_traces);
    }

    template<typename Archive>
    void load(Archive & ar,
              const unsigned int version) {
        size_t n_traces;

        ar & n_traces;
        m_traces.resize(n_traces);

        for (auto & trace : m_traces) {
            ar & trace;
        }

        m_traces.shrink_to_fit();
    }

    template<typename Archive>
    void save(Archive & ar,
              const unsigned int version) const {
        size_t n_traces = m_traces.size();

        ar & n_traces;

        for (auto & trace : m_traces) {
            ar & trace;
        }
    }
    BOOST_SERIALIZATION_SPLIT_MEMBER();
};

namespace std {
    template <>
    struct hash<TraceDB>
    {
        std::size_t operator()(const TraceDB & traceDB) const {
            return hash_value(traceDB);
        }
    };
}

#endif // __TRACE_DB_HPP
