//
// TraceVarInfo.hpp -- Immutable oject representing a variable and
// associated value recorded in the trace.  These objects are used
// as flyweights as many trace points will record the same variable
// value.
//

#ifndef __TRACE_VAR_INFO_HPP
#define __TRACE_VAR_INFO_HPP

#include <istream>
#include <ostream>
#include <string>
#include <vector>
#include <utility>

#include <boost/archive/basic_archive.hpp>
#include <boost/flyweight.hpp>
#include <boost/functional/hash.hpp>
#include <boost/serialization/access.hpp>
#include <boost/serialization/split_member.hpp>

#include "TypeDescription.hpp"
#include "MemoryMap.hpp"
#include "Utils.hpp"

class TraceVarInfo;

typedef boost::flyweights::flyweight<TraceVarInfo>
        FlyweightTraceVarInfo;
typedef std::vector<FlyweightTraceVarInfo>
        FlyweightTraceVarInfos;

typedef union {
    uint64_t u;
    int64_t s;
    float f;
    double d;
    void *ptr;
} VarValue;

class TraceVarInfo
{
private:
    friend class boost::serialization::access;

    // TraceVarInfo fields include the variable value,
    // variable name index, variable type index,
    // variable format, variable size, dynamically
    // allocated memory size, and a flag indicating
    // memory was dynamically allocated.
    //
    // Commentary: Two of these fields, m_format
    // and m_size are also fields on TypeDescription
    // objects.  The TypeDescription for the given
    // TraceVarInfo may be found by using the
    // type index on the parent trace's type vector.
    // Given this, one may ask - why not just include
    // the TypeDescription here?  The issue is that the
    // size field may be differ if the TraceVarInfo
    // object is a BLOB; thus, this field is required
    // regardless.  The format field is copied from
    // the TypeDescription.  To keep memory usage low,
    // only this field is copied instead of using
    // the whole TypeDescription object.  While we
    // could use a flyweight TypeDescription here so
    // that only a pointer is required, construction
    // of these objects is very costly in terms of CPU
    // time.  In short, a lot of options were considered,
    // and, in my view, the "best" is represented here.
    VarValue m_value;
    uint32_t m_name_index;
    uint32_t m_type_index;
    type_format m_format;
    uint32_t m_size;
    uint64_t m_buffer_size;
    uint8_t m_has_buffer_size;

    std::istream & read(std::istream & is,
                        const Names & names,
                        const TypeDescriptions & types) {
        return read(is, names, types, MemoryMap());
    }

    std::istream & read(std::istream & is,
                        const Names & names,
                        const TypeDescriptions & types,
                        const MemoryMap & map) {
        m_value.u = 0;
        m_name_index = 0;
        m_type_index = 0;
        m_format = INVALID_FORMAT;
        m_size = 0;
        m_buffer_size = 0;
        m_has_buffer_size = 0;

        BINARY_READ(is, &m_name_index, sizeof(m_name_index));
        BINARY_READ(is, &m_type_index, sizeof(m_type_index));

        if (m_type_index >= types.size()) {
            throw TraceError("Invalid type index.");
        }

        if (m_name_index >= names.size()) {
            throw TraceError("Invalid name index.");
        }

        m_format = types[m_type_index].getTypeFormat();
        m_size = types[m_type_index].getSize();
        m_value.u = 0;

        switch (m_format) {
        case SIGNED:
            {
                switch (m_size) {
                case 1:
                    {
                        int8_t tmp;
                        BINARY_READ(is, &tmp, m_size);
                        m_value.s = tmp;
                    }
                    break;
                case 2:
                    {
                        int16_t tmp;
                        BINARY_READ(is, &tmp, m_size);
                        m_value.s = tmp;
                    }
                    break;
                case 4:
                    {
                        int32_t tmp;
                        BINARY_READ(is, &tmp, m_size);
                        m_value.s = tmp;
                    }
                    break;
                case 8:
                    {
                        int64_t tmp;
                        BINARY_READ(is, &tmp, m_size);
                        m_value.s = tmp;
                    }
                    break;
                default:
                    throw TraceError("Invalid size for signed integer.");
                }
            }
            break;
        case UNSIGNED:
        case FLOAT:
            {
                BINARY_READ(is, &m_value.u, m_size);
            }
            break;
        case POINTER:
            {
                BINARY_READ(is, &m_value.u, m_size);
                m_buffer_size = map.computeBufferSize(m_value.u);
                if (m_buffer_size != UINT64_MAX) {
                    m_has_buffer_size = 1;
                } else {
                    m_buffer_size = 0;
                    m_has_buffer_size = 0;
                }
            }
            break;
        case BLOB:
            {
                /* Blob type: store value on the heap */
                if (m_size == 0) {
                    /* Variable-sized type: read size from trace */
                    BINARY_READ(is, &m_size, sizeof(m_size));
                }

                m_value.ptr = malloc(m_size);
                BINARY_READ(is, m_value.ptr, m_size);
                break;
            }
        case INVALID_FORMAT:
        default:
            throw TraceError("Invalid type format.");
        }

        return is;
    }

    std::ostream & write(std::ostream & os) const {
        BINARY_WRITE(os, &m_name_index, sizeof(m_name_index));
        BINARY_WRITE(os, &m_type_index, sizeof(m_type_index));

        switch (m_format) {
        case UNSIGNED:
            switch (m_size) {
            case 1:
                {
                    uint8_t val = m_value.u;
                    BINARY_WRITE(os, &val, sizeof(val));
                    break;
                }
            case 2:
                {
                    uint16_t val = m_value.u;
                    BINARY_WRITE(os, &val, sizeof(val));
                    break;
                }
            case 4:
                {
                    uint32_t val = m_value.u;
                    BINARY_WRITE(os, &val, sizeof(val));
                    break;
                }
            case 8:
                {
                    uint64_t val = m_value.u;
                    BINARY_WRITE(os, &val, sizeof(val));
                    break;
                }
            }
            break;
        case SIGNED:
            switch (m_size) {
            case 1:
                {
                    int8_t val = m_value.s;
                    BINARY_WRITE(os, &val, sizeof(val));
                    break;
                }
            case 2:
                {
                    int16_t val = m_value.s;
                    BINARY_WRITE(os, &val, sizeof(val));
                    break;
                }
            case 4:
                {
                    int32_t val = m_value.s;
                    BINARY_WRITE(os, &val, sizeof(val));
                    break;
                }
            case 8:
                {
                    int64_t val = m_value.s;
                    BINARY_WRITE(os, &val, sizeof(val));
                    break;
                }
            }
            break;
        case FLOAT:
            if (m_size == 4) {
                float val = m_value.f;
                BINARY_WRITE(os, &val, sizeof(val));
                break;
            }
            else {
                double val = m_value.d;
                BINARY_WRITE(os, &val, sizeof(val));
                break;
            }
            break;
        case POINTER:
            {
                void *val = m_value.ptr;
                BINARY_WRITE(os, &val, sizeof(val));
            }
            break;
        case BLOB:
            {
                void *val = m_value.ptr;
                BINARY_WRITE(os, &m_size, sizeof(m_size));
                BINARY_WRITE(os, val, m_size);
            }
            break;
        case INVALID_FORMAT:
        default:
            break;
        }

        return os;
    }

public:
    TraceVarInfo() :
        m_value(),
        m_name_index(0),
        m_type_index(0),
        m_format(INVALID_FORMAT),
        m_size(0),
        m_buffer_size(0),
        m_has_buffer_size(0)
    {}

    TraceVarInfo(VarValue value,
                 uint32_t name_index,
                 uint32_t type_index,
                 type_format format,
                 uint32_t size,
                 uint64_t buffer_size,
                 uint8_t has_buffer_size) :
        m_value(value),
        m_name_index(name_index),
        m_type_index(type_index),
        m_format(format),
        m_size(size),
        m_buffer_size(buffer_size),
        m_has_buffer_size(has_buffer_size)
    {}

    TraceVarInfo(const TraceVarInfo & other) :
        m_value(other.m_value),
        m_name_index(other.m_name_index),
        m_type_index(other.m_type_index),
        m_format(other.m_format),
        m_size(other.m_size),
        m_buffer_size(other.m_buffer_size),
        m_has_buffer_size(other.m_has_buffer_size)
    {}

    TraceVarInfo(std::istream & is,
                 const Names & names,
                 const TypeDescriptions & types) {
        read(is, names, types);
    }

    TraceVarInfo(std::istream & is,
                 const Names & names,
                 const TypeDescriptions & types,
                 const MemoryMap & map) {
        read(is, names, types, map);
    }

    TraceVarInfo& operator=(const TraceVarInfo & other) = delete;

    inline VarValue getValue() const {
        return m_value;
    }

    inline uint32_t getNameIndex() const {
        return m_name_index;
    }

    inline uint32_t getTypeIndex() const {
        return m_type_index;
    }

    inline type_format getTypeFormat() const {
        return m_format;
    }

    inline uint32_t getSize() const {
        return m_size;
    }

    inline uint64_t getBufferSize() const {
        return m_buffer_size;
    }

    inline uint8_t hasBufferSize() const {
        return m_has_buffer_size;
    }

    inline bool operator<(const TraceVarInfo & other) const {
        if (m_format == BLOB and other.m_format == BLOB) {
            std::vector<char> data({(char*) m_value.ptr,
                                    (char*) m_value.ptr + m_size});
            std::vector<char> otherData({(char*) other.m_value.ptr,
                                         (char*) other.m_value.ptr + m_size});
            return std::tie(data,
                            m_name_index,
                            m_type_index,
                            m_format,
                            m_size,
                            m_buffer_size,
                            m_has_buffer_size) <
                   std::tie(otherData,
                            other.m_name_index,
                            other.m_type_index,
                            other.m_format,
                            other.m_size,
                            other.m_buffer_size,
                            other.m_has_buffer_size);
        }
        else {
            return std::tie(m_value.u,
                            m_name_index,
                            m_type_index,
                            m_format,
                            m_size,
                            m_buffer_size,
                            m_has_buffer_size) <
                   std::tie(other.m_value.u,
                            other.m_name_index,
                            other.m_type_index,
                            other.m_format,
                            other.m_size,
                            other.m_buffer_size,
                            other.m_has_buffer_size);
        }
    }

    inline bool operator==(const TraceVarInfo & other) const {
        bool eq = true;

        eq &= m_name_index == other.m_name_index;
        eq &= m_type_index == other.m_type_index;
        eq &= m_format == other.m_format;
        eq &= m_size == other.m_size;
        eq &= m_buffer_size == other.m_buffer_size;
        eq &= m_has_buffer_size == other.m_has_buffer_size;

        if (eq) {
            if (m_format == BLOB) {
                eq &= (memcmp(m_value.ptr, other.m_value.ptr, m_size) == 0);
            }
            else if (m_size == 1) {
                uint8_t tmp1 = (uint8_t) m_value.u;
                uint8_t tmp2 = (uint8_t) other.m_value.u;
                eq &= tmp1 == tmp2;
            }
            else if (m_size == 2) {
                uint16_t tmp1 = (uint16_t) m_value.u;
                uint16_t tmp2 = (uint16_t) other.m_value.u;
                eq &= tmp1 == tmp2;
            }
            else if (m_size == 4) {
                uint32_t tmp1 = (uint32_t) m_value.u;
                uint32_t tmp2 = (uint32_t) other.m_value.u;
                eq &= tmp1 == tmp2;
            }
            else {
                eq &= m_value.u == other.m_value.u;
            }
        }

        return eq;
    }

    friend std::size_t hash_value(const TraceVarInfo & traceVarInfo) {
        std::size_t seed = 0;

        if (traceVarInfo.m_format == BLOB) {
            boost::hash_combine(seed,
                boost::hash_range((char*) traceVarInfo.m_value.ptr,
                                  (char*) traceVarInfo.m_value.ptr +
                                          traceVarInfo.m_size));
        }
        else if (traceVarInfo.m_size == 1) {
            boost::hash_combine(seed, (uint8_t) traceVarInfo.m_value.u);
        }
        else if (traceVarInfo.m_size == 2) {
            boost::hash_combine(seed, (uint16_t) traceVarInfo.m_value.u);
        }
        else if (traceVarInfo.m_size == 4) {
            boost::hash_combine(seed, (uint32_t) traceVarInfo.m_value.u);
        }
        else {
            boost::hash_combine(seed, traceVarInfo.m_value.u);
        }

        boost::hash_combine(seed, traceVarInfo.m_name_index);
        boost::hash_combine(seed, traceVarInfo.m_type_index);
        boost::hash_combine(seed, traceVarInfo.m_size);
        boost::hash_combine(seed, traceVarInfo.m_format);
        boost::hash_combine(seed, traceVarInfo.m_buffer_size);
        boost::hash_combine(seed, traceVarInfo.m_has_buffer_size);

        return seed;
    }

    friend std::ostream & operator<<(std::ostream & os,
                                     const TraceVarInfo & traceVarInfo) {
        return traceVarInfo.write(os);
    }

    template<typename Archive>
    void save(Archive & ar,
              const unsigned int version) const {
        ar & m_name_index;
        ar & m_type_index;
        ar & m_format;
        ar & m_size;
        ar & m_buffer_size;
        ar & m_has_buffer_size;

        if (m_format == BLOB) {
            for (uint32_t i = 0; i < m_size; i++)
                ar & ((char*) m_value.ptr)[i];
        }
        else {
            ar & m_value.u;
        }
    }

    template<typename Archive>
    void load(Archive & ar,
              const unsigned int version) {
        ar & m_name_index;
        ar & m_type_index;
        ar & m_format;
        ar & m_size;
        ar & m_buffer_size;
        ar & m_has_buffer_size;

        if (m_format == BLOB) {
            m_value.ptr = malloc(m_size);
            for (uint32_t i = 0; i < m_size; i++)
                ar & ((char*) m_value.ptr)[i];
        }
        else {
            ar & m_value.u;
        }
    }

    BOOST_SERIALIZATION_SPLIT_MEMBER()
};

namespace std {
    template <>
    struct hash<TraceVarInfo>
    {
        std::size_t operator()(const TraceVarInfo & traceVarInfo) const {
            return hash_value(traceVarInfo);
        }
    };
}

#endif // __TRACE_VAR_INFO_HPP
