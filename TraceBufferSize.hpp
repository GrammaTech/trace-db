//
// TraceBufferSize.hpp -- Immutable object representing a
// memory allocation recorded in the trace.  Contains a
// memory address and size.
//

#ifndef __TRACE_BUFFER_SIZE_HPP
#define __TRACE_BUFFER_SIZE_HPP

#include <istream>
#include <ostream>
#include <vector>
#include <utility>

#include <boost/archive/basic_archive.hpp>
#include <boost/functional/hash.hpp>
#include <boost/serialization/access.hpp>

#include "Utils.hpp"

class TraceBufferSize;

typedef std::vector<TraceBufferSize>
        TraceBufferSizes;

class TraceBufferSize
{
private:
    friend class boost::serialization::access;

    uint64_t m_address;
    uint64_t m_size;

    std::istream & read(std::istream & is) {
        m_address = 0;
        m_size = 0;

        BINARY_READ(is, &m_address, sizeof(m_address));
        BINARY_READ(is, &m_size, sizeof(m_size));

        return is;
    }

    std::ostream & write(std::ostream & os) const {
        BINARY_WRITE(os, &m_address, sizeof(m_address));
        BINARY_WRITE(os, &m_size, sizeof(m_size));
        return os;
    }

public:
    TraceBufferSize() :
        m_address(0),
        m_size(0)
    {}

    TraceBufferSize(uint64_t address,
                    uint64_t size) :
        m_address(address),
        m_size(size)
    {}

    TraceBufferSize(const TraceBufferSize & other) :
        m_address(other.m_address),
        m_size(other.m_size)
    {}

    TraceBufferSize(std::istream & is) {
        read(is);
    }

    TraceBufferSize& operator=(const TraceBufferSize & other) = delete;

    inline uint64_t getAddress() const {
        return m_address;
    }

    inline uint64_t getSize() const {
        return m_size;
    }

    inline bool operator<(const TraceBufferSize & other) const {
        return std::tie(m_address,
                        m_size) <
               std::tie(other.m_address,
                        other.m_size);
    }

    inline bool operator==(const TraceBufferSize & other) const {
        return m_address == other.m_address &&
               m_size == other.m_size;
    }

    friend std::size_t hash_value(const TraceBufferSize & traceBufferSize) {
        std::size_t seed = 0;

        boost::hash_combine(seed, traceBufferSize.m_address);
        boost::hash_combine(seed, traceBufferSize.m_size);

        return seed;
    }

    friend std::ostream & operator<<(std::ostream & os,
                                     const TraceBufferSize & traceBufferSize) {
        return traceBufferSize.write(os);
    }

    template<typename Archive>
    void serialize(Archive & ar,
                   const unsigned int version) {
        ar & m_address;
        ar & m_size;
    }
};

namespace std {
    template <>
    struct hash<TraceBufferSize>
    {
        std::size_t operator()(const TraceBufferSize & traceBufferSize) const {
            return hash_value(traceBufferSize);
        }
    };
}

#endif // __TRACE_BUFFER_SIZE_HPP
