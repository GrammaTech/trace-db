//
// TypeDescription.hpp -- Immutable object representing a type
// in the trace.  These objects are recorded at the beginning
// of a trace, and include a field for the name index,
// a format field, and a size field.
//

#ifndef __TYPE_DESCRIPTION_HPP
#define __TYPE_DESCRIPTION_HPP

#include <istream>
#include <ostream>
#include <string>
#include <vector>
#include <utility>

#include <boost/archive/basic_archive.hpp>
#include <boost/functional/hash.hpp>
#include <boost/serialization/access.hpp>

#include "Utils.hpp"

class TypeDescription;

typedef std::vector<std::string> Names;
typedef std::vector<TypeDescription> TypeDescriptions;

typedef enum type_format {
    UNSIGNED,                   /* unsigned integer */
    SIGNED,                     /* signed integer */
    FLOAT,                      /* floating point */
    POINTER,                    /* unsigned, interpret as address */
    BLOB,                       /* arbitrary bytes, do not interpret */
    INVALID_FORMAT
} type_format;

class TypeDescription
{
private:
    friend class boost::serialization::access;

    /* Index of the type name in the parent trace */
    uint32_t m_name_index;
    /* Data format */
    enum type_format m_format;
    /* Size in bytes. 0 indicates a variable-sized object. */
    uint32_t m_size;

    std::istream & read(std::istream & is) {
        m_name_index = 0;
        m_format = INVALID_FORMAT;
        m_size = 0;

        BINARY_READ(is, &m_name_index, sizeof(m_name_index));
        BINARY_READ(is, &m_format, sizeof(m_format));
        BINARY_READ(is, &m_size, sizeof(m_size));

        if (m_format >= INVALID_FORMAT) {
            throw TraceError("Invalid type format.");
        }

        return is;
    }

    std::istream & read(std::istream & is, const Names & names) {
        read(is);

        if (m_name_index >= names.size()) {
            throw TraceError("Invalid name index.");
        }

        return is;
    }

    std::ostream & write(std::ostream & os) const {
        BINARY_WRITE(os, &m_name_index, sizeof(m_name_index));
        BINARY_WRITE(os, &m_format, sizeof(m_format));
        BINARY_WRITE(os, &m_size, sizeof(m_size));

        return os;
    }

public:
    TypeDescription() :
        m_name_index(0),
        m_format(INVALID_FORMAT),
        m_size(0)
    {}

    TypeDescription(uint32_t name_index,
                    type_format format,
                    uint32_t size) :
        m_name_index(name_index),
        m_format(format),
        m_size(size)
    {}

    TypeDescription(const TypeDescription & other) :
        m_name_index(other.m_name_index),
        m_format(other.m_format),
        m_size(other.m_size)
    {}

    TypeDescription(std::istream & is) {
        read(is);
    }

    TypeDescription(std::istream & is,
                    const Names & names) {
        read(is, names);
    }

    TypeDescription& operator=(const TypeDescription & other) = delete;

    inline uint32_t getNameIndex() const {
        return m_name_index;
    }

    inline type_format getTypeFormat() const {
        return m_format;
    }

    inline uint32_t getSize() const {
        return m_size;
    }

    inline bool operator<(const TypeDescription & other) const {
        return std::tie(m_name_index,
                        m_format,
                        m_size) <
               std::tie(other.m_name_index,
                        other.m_format,
                        other.m_size);
    }

    inline bool operator==(const TypeDescription & other) const {
        return m_name_index == other.m_name_index &&
               m_format == other.m_format &&
               m_size == other.m_size;
    }

    friend std::size_t hash_value(const TypeDescription & typeDescription) {
        std::size_t seed = 0;

        boost::hash_combine(seed, typeDescription.m_name_index);
        boost::hash_combine(seed, typeDescription.m_format);
        boost::hash_combine(seed, typeDescription.m_size);

        return seed;
    }

    friend std::ostream & operator<<(std::ostream & os,
                                     const TypeDescription & typeDescription) {
        return typeDescription.write(os);
    }

    template<typename Archive>
    void serialize(Archive & ar,
                   const unsigned int version) {
        ar & m_name_index;
        ar & m_format;
        ar & m_size;
    }
};

namespace std {
    template <>
    struct hash<TypeDescription>
    {
        std::size_t operator()(const TypeDescription & typeDescription) const {
            return hash_value(typeDescription);
        }
    };
}

#endif // __TYPE_DESCRIPTION_HPP
