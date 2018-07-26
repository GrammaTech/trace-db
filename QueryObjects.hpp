//
// QueryObjects.hpp -- Collection of immutable objects for querying
// the TraceDatabase.  This includes objects representing a database
// predicate, free variables, and the intermediate results of query
// operations.
//

#ifndef __QUERY_OBJECTS_HPP
#define __QUERY_OBJECTS_HPP

#include <cassert>
#include <cstdint>
#include <ostream>

#include <boost/functional/hash.hpp>
#include <boost/multiprecision/cpp_bin_float.hpp>

class Predicate;
class FreeVariable;

typedef std::vector<Predicate>
        Predicates;
typedef std::vector<FreeVariable>
        FreeVariables;

enum predicate_kind
{
    /* NULL predicate */
    NULL_PREDICATE,
    /* Properties of variables */
    VAR_REFERENCE,
    VAR_SIZE,
    VAR_VALUE,
    DISTINCT_VARS,
    /* Numeric values */
    SIGNED_VALUE,
    UNSIGNED_VALUE,
    FLOAT_VALUE,
    /* Logical operators */
    AND,
    OR,
    NOT,
    /* Comparisons */
    GREATER_THAN,
    GREATER_THAN_OR_EQUAL,
    LESS_THAN,
    LESS_THAN_OR_EQUAL,
    EQUAL,
    /* Arithmetic operators */
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE
};

union predicate_data
{
    uint64_t n_children;
    uint64_t var_index;
    uint64_t unsigned_value;
    int64_t signed_value;
    double float_value;
};

class Predicate
{
private:
    enum predicate_kind m_kind;
    union predicate_data m_data;
    Predicates m_children;
    std::size_t m_hash;

    std::size_t computeHash() const {
        std::size_t seed = 0;

        boost::hash_combine(seed, m_kind);
        boost::hash_combine(seed, m_data.unsigned_value);
        boost::hash_combine(seed, m_children);

        return seed;
    }

public:
    Predicate() :
        m_kind(NULL_PREDICATE),
        m_data(),
        m_children(),
        m_hash(computeHash())
    {}

    Predicate(predicate_kind kind,
              predicate_data data,
              const Predicates & children = Predicates()) :
        m_kind(kind),
        m_data(data),
        m_children(children),
        m_hash(computeHash())
    {}

    Predicate(const Predicate & other) :
        m_kind(other.m_kind),
        m_data(other.m_data),
        m_children(other.m_children),
        m_hash(other.m_hash)
    {}

    Predicate & operator=(const Predicate & other) = delete;

    inline predicate_kind getKind() const {
        return m_kind;
    }

    inline predicate_data getData() const {
        return m_data;
    }

    inline const std::vector<Predicate> & getChildren() const {
        return m_children;
    }

    inline std::size_t getHash() const {
        return m_hash;
    }

    inline bool operator==(const Predicate & other) const {
        return m_kind == other.m_kind &&
               m_data.unsigned_value == other.m_data.unsigned_value &&
               m_children == other.m_children;
    }

    friend std::size_t hash_value(const Predicate & predicate) {
        return predicate.getHash();
    }

    friend std::ostream & writePredicate(std::ostream & os,
                                         const Predicate & predicate,
                                         unsigned indent = 0,
                                         bool do_indent = false) {
        if (predicate.getKind() == NULL_PREDICATE) {
            return os;
        }

        if (do_indent && indent) {
            for (unsigned i = 0; i < indent; i++) {
                os << " ";
            }
        }

        if (predicate.getKind() == VAR_REFERENCE) {
            os << "<v" << predicate.getData().var_index << ">";
        }
        else if (predicate.getKind() == SIGNED_VALUE) {
            os << predicate.getData().signed_value;
        }
        else if (predicate.getKind() == UNSIGNED_VALUE) {
            os << predicate.getData().unsigned_value;
        }
        else if (predicate.getKind() == FLOAT_VALUE) {
            os << predicate.getData().float_value;
        }
        else {
            std::string name;
            switch (predicate.getKind()) {
            case VAR_SIZE:
                name = "v/size";
                break;
            case VAR_VALUE:
                name = "v/value";
                break;
            case VAR_REFERENCE:
                name = "reference";
                break;
            case AND:
                name = "and";
                break;
            case OR:
                name = "or";
                break;
            case NOT:
                name = "not";
                break;
            case DISTINCT_VARS:
                name = "distinct";
                break;
            case LESS_THAN:
                name = "<";
                break;
            case GREATER_THAN:
                name = ">";
                break;
            case EQUAL:
                name = "=";
                break;
            case ADD:
                name = "+";
                break;
            case SUBTRACT:
                name = "-";
                break;
            case MULTIPLY:
                name = "*";
                break;
            case DIVIDE:
                name = "/";
                break;
            default:
                assert(false);
            }
            os << "(" << name << " ";
            indent += ("(" + name + " ").length();

            const std::vector<Predicate> & children = predicate.getChildren();
            for (uint32_t i = 0; i < children.size(); i++)
            {
                writePredicate(os, children[i], indent, i != 0);
                os << ((i == children.size() - 1) ? ")" : "\n");
            }
        }

        if (indent == 0)
            os << std::endl;

        return os;
    }

    friend std::ostream & operator<<(std::ostream & os,
                                     const Predicate &predicate) {
        writePredicate(os, predicate);
        return os;
    }
};

namespace std {
    template <>
    struct hash<Predicate>
    {
        std::size_t operator()(const Predicate & predicate) const {
            return predicate.getHash();
        }
    };
}

class FreeVariable
{
private:
    std::vector<uint32_t> m_allowed_types;
    std::size_t m_hash;

    std::size_t computeHash() const {
        boost::hash<std::vector<uint32_t>> hasher;
        return hasher(m_allowed_types);
    }

public:
    FreeVariable() :
        m_allowed_types(),
        m_hash(computeHash())
    {}

    FreeVariable(const std::vector<uint32_t> & allowed_types) :
        m_allowed_types(allowed_types),
        m_hash(computeHash())
    {}

    FreeVariable(const FreeVariable & other) :
        m_allowed_types(other.m_allowed_types),
        m_hash(other.m_hash)
    {}

    FreeVariable & operator=(const FreeVariable & other) = delete;

    inline const std::vector<uint32_t> & getAllowedTypes() const {
        return m_allowed_types;
    }

    inline std::size_t getHash() const {
        return m_hash;
    }

    inline bool operator==(const FreeVariable & other) const {
        return m_allowed_types == other.m_allowed_types;
    }

    friend std::size_t hash_value(const FreeVariable & freeVariable) {
        return freeVariable.getHash();
    }
};

namespace std {
    template <>
    struct hash<FreeVariable>
    {
        std::size_t operator()(const FreeVariable & freeVariable) const {
            return freeVariable.getHash();
        }
    };
}

class PredicateAndBindings
{
private:
    const Predicate & m_predicate;
    const std::vector<uint32_t> & m_bindings;
    std::size_t m_hash;

    std::size_t computeHash() const {
        std::size_t seed = 0;

        boost::hash_combine(seed, m_predicate);
        boost::hash_combine(seed, m_bindings);

        return seed;
    }

public:
    PredicateAndBindings(const Predicate & predicate,
                         const std::vector<uint32_t> & bindings) :
        m_predicate(predicate),
        m_bindings(bindings),
        m_hash(computeHash())
    {}

    PredicateAndBindings(const PredicateAndBindings & other) :
        m_predicate(other.m_predicate),
        m_bindings(other.m_bindings),
        m_hash(other.m_hash)
    {}

    PredicateAndBindings & operator=(const PredicateAndBindings & oth) = delete;

    inline const std::vector<uint32_t> & getBindings() const {
        return m_bindings;
    }

    inline const Predicate & getPredicate() const {
        return m_predicate;
    }

    inline std::size_t getHash() const {
        return m_hash;
    }

    inline bool operator==(const PredicateAndBindings & other) const {
        return m_predicate == other.m_predicate &&
               m_bindings == other.m_bindings;
    }

    friend std::size_t
    hash_value(const PredicateAndBindings & predicateAndBindings) {
        return predicateAndBindings.getHash();
    }
};

namespace std {
    template <>
    struct hash<PredicateAndBindings>
    {
        std::size_t
        operator()(const PredicateAndBindings & predicateAndBindings) const {
            return predicateAndBindings.getHash();
        }
    };
}

class PredicateValue
{
private:
    bool m_is_valid;
    boost::multiprecision::cpp_bin_float_50 m_value;

    static const boost::multiprecision::cpp_bin_float_50& EPSILON() {
        static const boost::multiprecision::cpp_bin_float_50 EPS("0.0001");
        return EPS;
    }

public:
    PredicateValue(const boost::multiprecision::cpp_bin_float_50 &val)
        : m_is_valid(true),
          m_value(val)
    {}

    PredicateValue() : m_is_valid(false)
    {}

    bool operator<(const PredicateValue & other) const {
        if (!(m_is_valid && other.m_is_valid))
            return false;
        return !(m_value == other.m_value) &&
               m_value < other.m_value;
    }

    bool operator<=(const PredicateValue & other) const {
        return (*this < other) || (*this == other);
    }

    bool operator>(const PredicateValue & other) const {
        if (!(m_is_valid && other.m_is_valid))
            return false;
        return !(m_value == other.m_value) &&
               m_value > other.m_value;
    }

    bool operator>=(const PredicateValue & other) const {
        return (*this > other) || (*this == other);
    }

    bool operator==(const PredicateValue & other) const {
        if (!(m_is_valid && other.m_is_valid))
            return false;
        return boost::multiprecision::abs(m_value - other.m_value) < EPSILON();
    }

    PredicateValue operator+(const PredicateValue & other) const {
        if (!(m_is_valid && other.m_is_valid))
            return PredicateValue();
        return PredicateValue(m_value + other.m_value);
    }

    PredicateValue operator-(const PredicateValue & other) const {
        if (!(m_is_valid && other.m_is_valid))
            return PredicateValue();
        return PredicateValue(m_value - other.m_value);
    }

    PredicateValue operator*(const PredicateValue & other) const {
        if (!(m_is_valid && other.m_is_valid))
            return PredicateValue();
        return PredicateValue(m_value * other.m_value);
    }

    PredicateValue operator/(const PredicateValue & other) const {
        if (!(m_is_valid && other.m_is_valid) || other.m_value == 0)
            return PredicateValue();
        return PredicateValue(m_value / other.m_value);
    }
};

#endif // __QUERY_OBJECTS_HPP
