#include <cassert>
#include <cfloat>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <sstream>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>

#include "TraceError.hpp"
#include "MemoryMap.hpp"
#include "Utils.hpp"
#include "TypeDescription.hpp"
#include "TraceBufferSize.hpp"
#include "TraceVarInfo.hpp"
#include "TracePoint.hpp"
#include "TraceDB.hpp"
#include "Trace.hpp"

#define TRACE_FILE "test/tmp.trace"
#define TIMEOUT 10

#define N_ELTS(array) sizeof(array)/sizeof(*array)

const char *failure_message = NULL;
const char *failure_file;
unsigned int failure_line;
unsigned int failure_count;

#define ASSERT(cond)                                    \
    do {                                                \
        if (!(cond))  {                                 \
            handle_failure(#cond, __FILE__, __LINE__);  \
            return;                                     \
        }                                               \
    } while (0)

#define ASSERT_TRACE_ERROR(body)                        \
    do {                                                \
        try {                                           \
            body;                                       \
            ASSERT(false);                              \
        }                                               \
        catch (TraceError &e) {                         \
            ASSERT(true);                               \
        }                                               \
    } while (0)

#define ASSERT_NO_TRACE_ERROR(body)                     \
    do {                                                \
        try {                                           \
            body;                                       \
            ASSERT(true);                               \
        }                                               \
        catch (TraceError &e) {                         \
            ASSERT(false);                              \
        }                                               \
    } while (0)

#define ASSERT_TRACE_EOF_ERROR(body)                    \
    do {                                                \
        try {                                           \
            body;                                       \
            ASSERT(false);                              \
        }                                               \
        catch (TraceEOFError &e) {                      \
            ASSERT(true);                               \
        }                                               \
    } while (0)

#define ASSERT_NO_TRACE_EOF_ERROR(body)                 \
    do {                                                \
        try {                                           \
            body;                                       \
            ASSERT(true);                               \
        }                                               \
        catch (TraceEOFError &e) {                      \
            ASSERT(false);                              \
        }                                               \
    } while (0)

void handle_failure(const char *message, const char *file, unsigned int line)
{
    failure_message = message;
    failure_file = file;
    failure_line = line;
}

#define RUN_TEST(name) run_test(&name, #name)

void run_test(void (*test_fun)(), const char *name)
{
    failure_message = NULL;
    test_fun();
    if (failure_message) {
        fprintf(stderr, "FAIL: %s\n  %s:%d: %s\n",
                name, failure_file, failure_line, failure_message);
        failure_count++;
    }
}

Names test_names({
    "fixed_blob",
    "var_blob",
    "int",
    "var",
    "x",
    "y",
    "z",
    "unsigned int",
    "big_positive",
    "big_negative",
    "int64_t",
    "uint64_t",
    "big_unsigned",
    "*void"
});

TypeDescriptions test_types({
    TypeDescription(2, SIGNED, sizeof(int)),
    TypeDescription(0, BLOB, 10),
    TypeDescription(1, BLOB, 0),
    TypeDescription(7, UNSIGNED, sizeof(unsigned int)),
    TypeDescription(10, SIGNED, sizeof(int64_t)),
    TypeDescription(11, UNSIGNED, sizeof(uint64_t)),
    TypeDescription(13, POINTER, sizeof(void *))
});

uint16_t namei(const std::string & name)
{
    for (size_t i = 0; i < test_names.size(); i++) {
        if (name == test_names[i])
            return i;
    }
    assert(0);
}

uint16_t typei(const std::string & name)
{
    for (size_t i = 0; i < test_types.size(); i++) {
        if (name == test_names[test_types[i].getNameIndex()])
            return i;
    }
    assert(0);
}

TraceBufferSize createTestTraceBufferSize()
{
    return TraceBufferSize(0x1000, 10);
}

Trace createTestTrace()
{
    unsigned int x = 0, y = 1;
    int z = -2;
    int64_t big_positive = INT64_MAX;
    int64_t big_negative = INT64_MIN;
    uint64_t big_unsigned = UINT64_MAX;
    void *voidptr = (void*)0x1234;
    VarValue value;
    FlyweightTraceVarInfos vars;
    TraceBufferSizes bufferSizes;
    Aux aux;
    TracePoints points;

    value.u = x;
    vars.push_back(
        FlyweightTraceVarInfo(value,
                              namei("x"),
                              typei("unsigned int"),
                              test_types[typei("unsigned int")].getTypeFormat(),
                              sizeof(x),
                              0,
                              0));
    value.u = y;
    vars.push_back(
        FlyweightTraceVarInfo(value,
                              namei("y"),
                              typei("unsigned int"),
                              test_types[typei("unsigned int")].getTypeFormat(),
                              sizeof(y),
                              0,
                              0));
    value.s = z;
    vars.push_back(
        FlyweightTraceVarInfo(value,
                              namei("z"),
                              typei("int"),
                              test_types[typei("int")].getTypeFormat(),
                              sizeof(y),
                              0,
                              0));
    value.s = big_positive;
    vars.push_back(
        FlyweightTraceVarInfo(value,
                              namei("big_positive"),
                              typei("int64_t"),
                              test_types[typei("int64_t")].getTypeFormat(),
                              sizeof(big_positive),
                              0,
                              0));
    value.s = big_negative;
    vars.push_back(
        FlyweightTraceVarInfo(value,
                              namei("big_negative"),
                              typei("int64_t"),
                              test_types[typei("int64_t")].getTypeFormat(),
                              sizeof(big_negative),
                              0,
                              0));
    value.u = big_unsigned;
    vars.push_back(
        FlyweightTraceVarInfo(value,
                              namei("big_unsigned"),
                              typei("uint64_t"),
                              test_types[typei("uint64_t")].getTypeFormat(),
                              sizeof(big_unsigned),
                              0,
                              0));
    value.ptr = voidptr;
    vars.push_back(
        FlyweightTraceVarInfo(value,
                              namei("var"),
                              typei("*void"),
                              test_types[typei("*void")].getTypeFormat(),
                              sizeof(voidptr),
                              4,
                              1));

    bufferSizes.push_back(TraceBufferSize(0x1234, 4));

    points.push_back(TracePoint(1, 1, bufferSizes, vars, aux));

    return Trace(test_names, test_types, points);
}

void test_eof_in_header()
{
    /* Empty file */
    {
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        Trace t;
        ASSERT_TRACE_EOF_ERROR(Trace tmp(in));
        in.close();
    }

    /* Truncated name dictionary */
    {
        uint16_t val[] = { 100, 0, 0, 0 };
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
        out.write((char*) &val, sizeof(val));
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        Trace t;
        ASSERT_TRACE_EOF_ERROR(Trace tmp(in));
        in.close();
    }

    /* Truncated type dictionary */
    {
        const char *name = "foo";
        uint64_t val = strlen(name) + 1;
        uint16_t vals[] = { 10, 0, 0, 0 };
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);

        out.write((char*) &val, sizeof(val));
        out.write(name, val);
        out.write((char*) &vals, sizeof(vals));
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        Trace t;
        ASSERT_TRACE_EOF_ERROR(Trace tmp(in));
        in.close();
    }
}

void test_good_header()
{
    Trace t1(test_names, test_types, std::vector<TracePoint>());
    std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
    out << t1;
    out.close();

    std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
    Trace t2(in);
    in.close();

    ASSERT(t2.getNames() == test_names);
    ASSERT(t2.getTypes() == test_types);
}

void test_bad_tag()
{
    Trace t(test_names, test_types, std::vector<TracePoint>());
    std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
    out << t;
    out.put(INVALID_TAG + 2);
    out.close();

    std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
    ASSERT_TRACE_ERROR(Trace tmp(in));
    in.close();
}

void test_eof_in_tag()
{
    Trace t1(test_names, test_types, std::vector<TracePoint>());
    TracePoint p;

    std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
    out << t1;
    out.close();

    std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
    Trace t2(in);
    ASSERT_TRACE_EOF_ERROR(TracePoint p(in, t2.getNames(), t2.getTypes()));
    in.close();
}

void test_eof_in_id()
{
    Trace t(test_names, test_types, std::vector<TracePoint>());
    std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
    out << t;
    out.put(STATEMENT_ID);
    out.put(0xab);
    out.close();

    std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
    ASSERT_TRACE_EOF_ERROR(Trace tmp(in));
    in.close();
}

void test_eof_in_variable()
{
    {
        /* EOF in name index */
        Trace t(test_names, test_types, std::vector<TracePoint>());
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
        out << t;
        out.put(VARIABLE);
        out.put(0xab); /* arbitrary byte */
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        ASSERT_TRACE_EOF_ERROR(Trace tmp(in));
        in.close();
    }

    {
        /* EOF in var index */
        Trace t(test_names, test_types, std::vector<TracePoint>());
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
        out << t;
        out.put(VARIABLE);
        out.put(0);
        out.put(0);
        out.put(0xab); /* arbitrary byte */
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        ASSERT_TRACE_EOF_ERROR(Trace tmp(in));
        in.close();
    }

    {
        /* EOF in data */
        Trace t(test_names, test_types, std::vector<TracePoint>());
        uint32_t name_index = 0;
        uint32_t type_index = 0;
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
        out << t;
        out.put(VARIABLE);
        out.write((char*) &name_index, sizeof(name_index));
        out.write((char*) &type_index, sizeof(type_index));
        out.put(0xab); /* arbitrary byte */
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        ASSERT_TRACE_EOF_ERROR(Trace tmp(in));
        in.close();
    }
}

void test_eof_in_blob_variable()
{
    {
        /* EOF in fixed data */
        Trace t(test_names, test_types, std::vector<TracePoint>());
        uint32_t name_index = 1;
        uint32_t type_index = 1;
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
        out << t;
        out.put(VARIABLE);
        out.write((char*) &name_index, sizeof(name_index));
        out.write((char*) &type_index, sizeof(type_index));
        out.put(0xab); /* arbitrary byte */
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        ASSERT_TRACE_EOF_ERROR(Trace tmp = Trace(in));
        in.close();
    }

    {
        /* EOF in size */
        Trace t(test_names, test_types, std::vector<TracePoint>());
        uint32_t name_index = 2;
        uint32_t type_index = 2;
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
        out << t;
        out.put(VARIABLE);
        out.write((char*) &name_index, sizeof(name_index));
        out.write((char*) &type_index, sizeof(type_index));
        out.put(0xab); /* arbitrary byte */
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        ASSERT_TRACE_EOF_ERROR(Trace tmp(in));
        in.close();
    }

    {
        /* EOF in variable data */
        Trace t(test_names, test_types, std::vector<TracePoint>());
        uint32_t name_index = 2;
        uint32_t type_index = 2;
        uint16_t size = 3;
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
        out << t;
        out.put(VARIABLE);
        out.write((char*) &name_index, sizeof(name_index));
        out.write((char*) &type_index, sizeof(type_index));
        out.write((char*) &size, sizeof(size)); /* data size */
        out.put(0xab); /* arbitrary byte */
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        ASSERT_TRACE_EOF_ERROR(Trace tmp(in));
        in.close();
    }
}

void test_bad_index_in_variable()
{
    {
        /* Bad name index */
        uint64_t statement = 0;
        uint32_t name_index = test_names.size();
        uint32_t type_index = 0;
        uint32_t value = 0;
        Trace t(test_names, test_types, std::vector<TracePoint>());
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
        out << t;
        out.put(STATEMENT_ID);
        out.write((char*) &statement, sizeof(statement));
        out.put(VARIABLE);
        out.write((char*) &name_index, sizeof(name_index));
        out.write((char*) &type_index, sizeof(type_index));
        out.write((char*) &value, sizeof(value));
        out.put(END_ENTRY);
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        ASSERT_TRACE_ERROR(Trace tmp(in));
        in.close();
    }

    {
        uint64_t statement = 0;
        uint32_t name_index = 0;
        uint32_t type_index = test_types.size();
        Trace t(test_names, test_types, std::vector<TracePoint>());
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
        out.put(STATEMENT_ID);
        out.write((char*) &statement, sizeof(statement));
        out.put(VARIABLE);
        out.write((char*) &name_index, sizeof(name_index));
        out.write((char*) &type_index, sizeof(type_index));
        out << t;
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        ASSERT_TRACE_ERROR(Trace tmp(in));
        in.close();
    }
}

void test_bad_type()
{
    /* Invalid format */
    {
        uint32_t name_index = 0;
        enum type_format format = INVALID_FORMAT;
        Trace t(test_names, TypeDescriptions(), TracePoints());
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
        out << t;
        BINARY_WRITE(out, &name_index, sizeof(name_index));
        BINARY_WRITE(out, &format, sizeof(format));
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        ASSERT_TRACE_ERROR(Trace tmp(in));
        in.close();
    }

    /* Bad name index */
    {
        uint32_t name_index = 9999;
        Trace t(test_names, TypeDescriptions(), TracePoints());
        std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
        out << t;
        BINARY_WRITE(out, &name_index, sizeof(name_index));
        out.close();

        std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
        ASSERT_TRACE_ERROR(Trace tmp(in));
        in.close();
    }
}

void test_eof_in_buffer_size()
{
    std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
    char dummy = 0xab;
    out.write(&dummy, sizeof(dummy));
    out.close();

    std::ifstream in(TRACE_FILE, std::ios::in | std::ios::binary);
    ASSERT_TRACE_EOF_ERROR(TraceBufferSize traceBufferSize(in));
    in.close();
}

void test_read_from_fifo()
{
    Trace t(test_names, test_types, std::vector<TracePoint>());
    std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
    out << t;
    out.close();

    mkfifo("test/fifo", 0700);
    ASSERT(system("cat test/tmp.trace > test/fifo &") == 0);

    boost::iostreams::stream_buffer
        <boost::iostreams::file_descriptor_source>* fpstream
        = openWithTimeout("test/fifo", TIMEOUT);

    ASSERT_NO_TRACE_ERROR(Trace tmp(std::istream(fpstream)));
    delete fpstream;
    unlink("test/fifo");
}

void test_timeout_from_fifo()
{
    mkfifo("test/fifo", 0700);

    ASSERT_TRACE_ERROR(openWithTimeout("test/fifo", 1));
    unlink("test/fifo");
}

void test_memory_map()
{
    MemoryMap m;

    m.updateMemoryMap(TraceBufferSize(3, 10));
    m.updateMemoryMap(TraceBufferSize(100, 2));

    /* Basic lookups */
    ASSERT(m.computeBufferSize(3) == 10);
    ASSERT(m.computeBufferSize(100) == 2);

    /* Pointers in the middle of the buffer */
    ASSERT(m.computeBufferSize(8) == 5);
    ASSERT(m.computeBufferSize(101) == 1);

    /* End of buffer */
    ASSERT(m.computeBufferSize(13) == 0);
    ASSERT(m.computeBufferSize(102) == 0);

    /* Pointers outside known buffers */
    ASSERT(m.computeBufferSize(99) == UINT64_MAX);
    ASSERT(m.computeBufferSize(2) == UINT64_MAX);

    /* New allocation supersedes old */
    m.updateMemoryMap(TraceBufferSize(3, 5));
    ASSERT(m.computeBufferSize(3) == 5);

    /* Freed memory is removed from the map */
    m.updateMemoryMap(TraceBufferSize(100, 0));
    ASSERT(m.computeBufferSize(100) == UINT64_MAX);

    /* UINT64_MAX is never found in map (used as a sentinel) */
    ASSERT(m.computeBufferSize(UINT64_MAX) == UINT64_MAX);

    /* Inserting UINT64_MAX does nothing */
    m.updateMemoryMap(TraceBufferSize(UINT64_MAX, 5));
    ASSERT(m.computeBufferSize(UINT64_MAX) == UINT64_MAX);
}

void test_query_variable_binding()
{
    Trace t(createTestTrace());

    TracePoints results;
    uint32_t type_a = typei("unsigned int");
    uint32_t type_b = typei("int");
    FreeVariables vars({ FreeVariable(std::vector<uint32_t>({type_a})),
                         FreeVariable(std::vector<uint32_t>({type_b})) });

    results = t.query(vars, Predicate());

    ASSERT(results.size() == 2);

    TracePoint r1 = *results.begin();
    TracePoint r2 = *results.rbegin();

    ASSERT(r1.getVars().size() == 2);
    ASSERT(r1.getVars()[0].get().getNameIndex() == namei("x"));
    ASSERT(r1.getVars()[0].get().getValue().u == 0);
    ASSERT(r1.getVars()[1].get().getNameIndex() == namei("z"));
    ASSERT(r1.getVars()[1].get().getValue().s == -2);

    ASSERT(r2.getVars().size() == 2);
    ASSERT(r2.getVars()[0].get().getNameIndex() == namei("y"));
    ASSERT(r2.getVars()[0].get().getValue().u == 1);
    ASSERT(r2.getVars()[1].get().getNameIndex() == namei("z"));
    ASSERT(r2.getVars()[1].get().getValue().s == -2);
}

void test_query_predicates()
{
    Trace t(createTestTrace());

    TracePoints results;
    uint32_t type_a = typei("unsigned int");
    uint32_t type_b = typei("int");
    uint32_t type_i64 = typei("int64_t");
    uint32_t int_types[] = { type_a, type_b };
    FreeVariables int_vars({
        FreeVariable(std::vector<uint32_t>(int_types,
                                           int_types + N_ELTS(int_types))),
        FreeVariable(std::vector<uint32_t>(int_types,
                                           int_types + N_ELTS(int_types)))
    });

    Predicate var1(VAR_REFERENCE, {0});
    Predicate var2(VAR_REFERENCE, {1});
    Predicate var1_val(VAR_VALUE, {1}, Predicates({var1}));
    Predicate var2_val(VAR_VALUE, {1}, Predicates({var2}));
    Predicates vars({ var1, var2 });
    Predicates var_values({ var1_val, var2_val });
    Predicate distinct_vars(DISTINCT_VARS, {2}, vars);
    Predicate negative_one(SIGNED_VALUE, {(uint64_t) -1});
    Predicate negative_two(SIGNED_VALUE, {(uint64_t) -2});
    Predicate zero(SIGNED_VALUE, {0});
    Predicate three(SIGNED_VALUE, {3});
    Predicate int64_max(SIGNED_VALUE, {(uint64_t) INT64_MAX});
    Predicate uint64_max(UNSIGNED_VALUE, {UINT64_MAX});
    Predicate int64_min(SIGNED_VALUE, {(uint64_t) INT64_MIN});

    /* Distinct variables */
    {
        FreeVariables vars({ FreeVariable(std::vector<uint32_t>({type_a})),
                             FreeVariable(std::vector<uint32_t>({type_a})) });

        /* Unrestricted query should return 4 results, two bindings for each
         * variable */
        results = t.query(vars, Predicate());
        ASSERT(results.size() == 4);

        /* With DISTINCT_VARS predicate, only two results are valid */
        results = t.query(vars, distinct_vars);
        ASSERT(results.size() == 2);
    }

    /* Greater than, unsigned constant */
    {
        FreeVariables vars({
            FreeVariable(std::vector<uint32_t>(int_types,
                                               int_types + N_ELTS(int_types)))
        });

        /* a > 0 */
        Predicates p0({ var1_val, zero });
        Predicate p(GREATER_THAN, {2}, p0);

        results = t.query(vars, p);
        ASSERT(results.size() == 1);
    }

    /* Less than, signed constant */
    {
        FreeVariables vars({
            FreeVariable(std::vector<uint32_t>(int_types,
                                               int_types + N_ELTS(int_types)))
        });

        /* a < -1 */
        Predicates p0({ var1_val, negative_one });
        Predicate p(LESS_THAN, {2}, p0);

        results = t.query(vars, p);
        ASSERT(results.size() == 1);
    }

    /* Addition */
    {
        /* a + b == -1 */
        Predicates p0({ Predicate(ADD, {2}, var_values), negative_one });
        Predicate p(EQUAL, {2}, p0);

        results = t.query(int_vars, p);
        ASSERT(results.size() == 2);
        ASSERT(results[0].getVars()[0].get().getNameIndex() == namei("z"));
        ASSERT(results[0].getVars()[1].get().getNameIndex() == namei("y"));
    }

    /* Subtraction */
    {
        /* a - b == 3 */
        Predicates p0({ Predicate(SUBTRACT, {2}, var_values), three });
        Predicate p(EQUAL, {2}, p0);

        results = t.query(int_vars, p);
        ASSERT(results.size() == 1);
        ASSERT(results[0].getVars()[0].get().getNameIndex() == namei("y"));
        ASSERT(results[0].getVars()[1].get().getNameIndex() == namei("z"));
    }

    /* Multiplication */
    {
        Predicate zero(SIGNED_VALUE, {0});

        /* a * b == 0 */
        Predicates p0({ Predicate(MULTIPLY, {2}, var_values), zero });
        Predicate p(EQUAL, {2}, p0);

        results = t.query(int_vars, p);
        ASSERT(results.size() == 5);
        for (auto & result : results)
            ASSERT(result.getVars()[0].get().getNameIndex() == namei("x") ||
                   result.getVars()[1].get().getNameIndex() == namei("x"));
    }

    /* Division */
    {
        /* a / b == 0 */
        Predicates p0({ Predicate(DIVIDE, {2}, var_values), zero });
        Predicate p(EQUAL, {2}, p0);

        results = t.query(int_vars, p);
        ASSERT(results.size() == 2);
        ASSERT(results[0].getVars()[0].get().getNameIndex() == namei("x"));
    }

    /* And, or */
    {
        FreeVariables vars({
            FreeVariable(std::vector<uint32_t>(int_types,
                                               int_types + N_ELTS(int_types)))
        });

        /* a > -0 */
        Predicates p0({ var1_val, zero });
        Predicate gt(GREATER_THAN, {2}, p0);

        /* a < 3 */
        Predicates p1({ var1_val, three });
        Predicate lt(LESS_THAN, {2}, p1);

        /* a == -2 */
        Predicates p2({ var1_val, negative_two });
        Predicate eq(EQUAL, {2}, p2);

        Predicates p3({ gt, lt });
        Predicate pand(AND, {2}, p3);

        Predicates p4({ pand, eq });
        Predicate por(OR, {2}, p4);

        results = t.query(vars, por);
        ASSERT(results.size() == 2);
        ASSERT(results[0].getVars()[0].get().getNameIndex() == namei("y"));
        ASSERT(results[1].getVars()[0].get().getNameIndex() == namei("z"));
    }

    /* Overflow check */
    {
        /* a + b > INT64_MAX */
        Predicates p0({ Predicate(ADD, {2}, var_values), int64_max });
        Predicate p(GREATER_THAN, {2}, p0);
        FreeVariables vars({ FreeVariable(std::vector<uint32_t>({type_i64})),
                             FreeVariable(std::vector<uint32_t>({type_i64})) });

        results = t.query(vars, p);
        ASSERT(results.size() == 1);
        ASSERT(results[0].getVars()[0].get().getNameIndex() ==
               namei("big_positive"));
    }

    /* Underflow check */
    {
        /* a + b < INT64_MIN */
        Predicates p0({ Predicate(ADD, {2}, var_values), int64_min });
        Predicate p(LESS_THAN, {2}, p0);
        FreeVariables vars({ FreeVariable(std::vector<uint32_t>({type_i64})),
                             FreeVariable(std::vector<uint32_t>({type_i64})) });

        results = t.query(vars, p);
        ASSERT(results.size() == 1);
        ASSERT(results[0].getVars()[0].get().getNameIndex() ==
               namei("big_negative"));
    }

    /* Signed overflow check */
    {
        /* a + b > UINT64_MAX */
        Predicates p0({ Predicate(ADD, {2}, var_values), uint64_max });
        Predicate p(GREATER_THAN, {2}, p0);
        uint32_t type_u64 = typei("uint64_t");
        FreeVariables vars({ FreeVariable(std::vector<uint32_t>({type_u64})),
                             FreeVariable(std::vector<uint32_t>({type_u64})) });

        results = t.query(vars, p);
        ASSERT(results.size() == 1);
        ASSERT(results[0].getVars()[0].get().getNameIndex() ==
               namei("big_unsigned"));
    }
}

void test_query_var_sizes()
{
    Trace t(createTestTrace());

    TracePoints results;

    uint32_t type_a = typei("*void");
    FreeVariables vars({ FreeVariable(std::vector<uint32_t>({type_a}))});

    Predicate var(VAR_REFERENCE, {0});
    Predicate four(UNSIGNED_VALUE, {4});
    Predicate five(UNSIGNED_VALUE, {5});

    /* Unrestricted query should return 1 result */
    {
        results = t.query(vars, Predicate());

        ASSERT(results.size() == 1);
    }

    /* size == 4, one result */
    {
        Predicates p0({ Predicate(VAR_SIZE, {1}, Predicates({var})),
                        four });
        Predicate p(EQUAL, {2}, p0);
        results = t.query(vars, p);

        ASSERT(results.size() == 1);
    }

    /* size == 5, no results */
    {
        Predicates p0({ Predicate(VAR_SIZE, {1}, Predicates({var})),
                        five });
        Predicate p(EQUAL, {2}, p0);
        results = t.query(vars, p);

        ASSERT(results.size() == 0);
    }
}

void test_query_soft_predicates()
{
    Trace t(createTestTrace());

    TracePoints results;

    uint32_t types[] = { typei("int"), typei("unsigned int") };
    FreeVariables vars({
        FreeVariable(std::vector<uint32_t>(types, types + N_ELTS(types))),
        FreeVariable(std::vector<uint32_t>(types, types + N_ELTS(types)))
    });
    Predicate var1(VAR_REFERENCE, {0});
    Predicate var1_val(VAR_VALUE, {1}, Predicates({var1}));

    Predicate negative_two(SIGNED_VALUE, {(uint64_t) -2});
    Predicate one(SIGNED_VALUE, {1});

    /* Unrestricted: 9 matches */
    {
        results = t.query(vars, Predicate());

        ASSERT(results.size() == 9);
    }

    /* Single predicate: x and z both satisfy it */
    {
        Predicates p0({ var1_val, one });
        Predicate p(LESS_THAN, {2}, p0);
        Predicates soft({ p });

        results = t.query(vars, Predicate(), soft);

        ASSERT(results.size() == 6);
        for (auto & result : results)
            ASSERT(result.getVars()[0].get().getNameIndex() == namei("z") ||
                   result.getVars()[0].get().getNameIndex() == namei("x"));
    }

    /* Two predicates: only z satisfies both */
    {
        Predicates p0({ var1_val, one });
        Predicate s0(LESS_THAN, {2}, p0);
        Predicates p1({ var1_val, negative_two });
        Predicate s1(EQUAL, {2}, p1);
        Predicates soft({ s0, s1 });

        results = t.query(vars, Predicate(), soft);

        ASSERT(results.size() == 3);
        for (auto & result : results)
            ASSERT(result.getVars()[0].get().getNameIndex() == namei("z"));
    }
}

void test_trace_buffer_size_input_output ()
{
    TraceBufferSize traceBufferSize1 = createTestTraceBufferSize();

    std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
    out << traceBufferSize1;
    out.close();

    std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
    TraceBufferSize traceBufferSize2(in);
    in.close();

    ASSERT(traceBufferSize1 == traceBufferSize2);
    ASSERT(hash_value(traceBufferSize1) == hash_value(traceBufferSize2));
}

void test_trace_buffer_size_serialization ()
{
    std::ostringstream oss;
    boost::archive::text_oarchive oa(oss);

    TraceBufferSize serialized = createTestTraceBufferSize();
    TraceBufferSize deserialized;

    oa & serialized;

    std::istringstream iss(oss.str());
    boost::archive::text_iarchive ia(iss);

    ia & deserialized;

    ASSERT(serialized == deserialized);
    ASSERT(hash_value(serialized) == hash_value(deserialized));
}

void test_type_description_input_output ()
{
    Names names({ "unsigned" });
    TypeDescription typeDescription1(0, UNSIGNED, 2);

    std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
    out << typeDescription1;
    out.close();

    std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
    TypeDescription typeDescription2(in, names);
    in.close();

    ASSERT(typeDescription1 == typeDescription2);
    ASSERT(hash_value(typeDescription1) == hash_value(typeDescription2));
}

void test_type_description_serialization ()
{
    std::ostringstream oss;
    boost::archive::text_oarchive oa(oss);

    TypeDescription serialized(0, SIGNED, 2);
    TypeDescription deserialized;

    oa & serialized;

    std::istringstream iss(oss.str());
    boost::archive::text_iarchive ia(iss);

    ia & deserialized;

    ASSERT(serialized == deserialized);
    ASSERT(hash_value(serialized) == hash_value(deserialized));
}

void test_trace_var_info_input_output () {
    VarValue value1;
    VarValue value2;
    VarValue value3;
    VarValue value4;
    VarValue value5;

    value1.u = UINT32_MAX;
    value2.s = INT32_MAX;
    value3.f = FLT_MIN;
    value4.d = DBL_MIN;
    value5.ptr = &value1;

    Names names({
        "a",
        "b",
        "c",
        "d",
        "e",
        "unsigned",
        "signed",
        "float",
        "double",
        "int*",
    });
    TypeDescriptions types({
        TypeDescription(5, UNSIGNED, sizeof(uint32_t)),
        TypeDescription(6, SIGNED, sizeof(int32_t)),
        TypeDescription(7, FLOAT, sizeof(float)),
        TypeDescription(8, FLOAT, sizeof(double)),
        TypeDescription(9, POINTER, sizeof(int*))
    });
    TraceVarInfo var1(value1, 0, 0,
                      types[0].getTypeFormat(),
                      types[0].getSize(),
                      0u, 0u);
    TraceVarInfo var2(value2, 1, 1,
                      types[1].getTypeFormat(),
                      types[1].getSize(),
                      0u, 0u);
    TraceVarInfo var3(value3, 2, 2,
                      types[2].getTypeFormat(),
                      types[2].getSize(),
                      0u, 0u);
    TraceVarInfo var4(value4, 3, 3,
                      types[3].getTypeFormat(),
                      types[3].getSize(),
                      0u, 0u);
    TraceVarInfo var5(value5, 4, 4,
                      types[4].getTypeFormat(),
                      types[4].getSize(),
                      0u, 0u);

    std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
    out << var1;
    out << var2;
    out << var3;
    out << var4;
    out << var5;
    out.close();

    std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
    TraceVarInfo var6(in, names, types);
    TraceVarInfo var7(in, names, types);
    TraceVarInfo var8(in, names, types);
    TraceVarInfo var9(in, names, types);
    TraceVarInfo var10(in, names, types);
    in.close();

    ASSERT(var1 == var6);
    ASSERT(var2 == var7);
    ASSERT(var3 == var8);
    ASSERT(var4 == var9);
    ASSERT(var5 == var10);
    ASSERT(hash_value(var1) == hash_value(var6));
    ASSERT(hash_value(var2) == hash_value(var7));
    ASSERT(hash_value(var3) == hash_value(var8));
    ASSERT(hash_value(var4) == hash_value(var9));
    ASSERT(hash_value(var5) == hash_value(var10));
    ASSERT(var6.getValue().u == UINT32_MAX);
    ASSERT(var7.getValue().s == INT32_MAX);
    ASSERT(var8.getValue().f == FLT_MIN);
    ASSERT(var9.getValue().d == DBL_MIN);
    ASSERT(var10.getValue().ptr == &value1);
}
void test_trace_var_info_serialization ()
{
    std::ostringstream oss;
    boost::archive::text_oarchive oa(oss);

    uint32_t name_index      = 2u;
    uint32_t type_index      = 3u;
    type_format format       = SIGNED;
    uint32_t size            = 4u;
    uint32_t buffer_size     = 5u;
    uint8_t  has_buffer_size = 1;
    TypeDescription type(name_index, format, size);

    VarValue value1;
    VarValue value2;
    VarValue value3;
    VarValue value4;
    VarValue value5;
    VarValue value6;

    value1.u = UINT32_MAX;
    value2.s = INT32_MAX;
    value3.f = FLT_MIN;
    value4.d = DBL_MIN;
    value5.ptr = &name_index;
    value6.ptr = (void*) "hello";

    TraceVarInfo serialized1(value1, name_index, type_index, format, size,
                             buffer_size, has_buffer_size);
    TraceVarInfo serialized2(value2, name_index, type_index, format, size,
                             buffer_size, has_buffer_size);
    TraceVarInfo serialized3(value3, name_index, type_index, format, size,
                             buffer_size, has_buffer_size);
    TraceVarInfo serialized4(value4, name_index, type_index, format, size,
                             buffer_size, has_buffer_size);
    TraceVarInfo serialized5(value5, name_index, type_index, format, size,
                             buffer_size, has_buffer_size);
    TraceVarInfo serialized6(value6, name_index, type_index,
                             BLOB, strlen("hello")+1,
                             buffer_size, has_buffer_size);
    TraceVarInfo deserialized1;
    TraceVarInfo deserialized2;
    TraceVarInfo deserialized3;
    TraceVarInfo deserialized4;
    TraceVarInfo deserialized5;
    TraceVarInfo deserialized6;

    oa & serialized1 & serialized2 & serialized3
       & serialized4 & serialized5 & serialized6;

    std::istringstream iss(oss.str());
    boost::archive::text_iarchive ia(iss);
    ia & deserialized1 & deserialized2 & deserialized3
       & deserialized4 & deserialized5 & deserialized6;

    ASSERT(serialized1 == deserialized1);
    ASSERT(serialized2 == deserialized2);
    ASSERT(serialized3 == deserialized3);
    ASSERT(serialized4 == deserialized4);
    ASSERT(serialized5 == deserialized5);
    ASSERT(serialized6 == deserialized6);
    ASSERT(hash_value(serialized1) == hash_value(deserialized1));
    ASSERT(hash_value(serialized2) == hash_value(deserialized2));
    ASSERT(hash_value(serialized3) == hash_value(deserialized3));
    ASSERT(hash_value(serialized4) == hash_value(deserialized4));
    ASSERT(hash_value(serialized5) == hash_value(deserialized5));
    ASSERT(hash_value(serialized6) == hash_value(deserialized6));
    ASSERT(deserialized1.getValue().u == UINT32_MAX);
    ASSERT(deserialized2.getValue().s == INT32_MAX);
    ASSERT(deserialized3.getValue().f == FLT_MIN);
    ASSERT(deserialized4.getValue().d == DBL_MIN);
    ASSERT(deserialized5.getValue().ptr == &name_index);
    ASSERT(memcmp(deserialized6.getValue().ptr, "hello", strlen("hello")) == 0);
}

void test_trace_point_serialization ()
{
    std::ostringstream oss;
    boost::archive::text_oarchive oa(oss);

    Names names({
        "name1",
        "name2"
    });
    TypeDescriptions types({
        TypeDescription(0u, UNSIGNED, 4u),
        TypeDescription(1u, SIGNED, 4u)
    });
    TraceBufferSizes sizes;
    FlyweightTraceVarInfos vars;
    Aux aux;

    VarValue value;
    value.u = 1u;
    sizes.push_back(TraceBufferSize(1u, 2u));
    vars.push_back(FlyweightTraceVarInfo(value, 1u, 1u,
                                         types[1u].getTypeFormat(),
                                         types[1u].getSize(),
                                         8u, 1u));
    aux.push_back(32u);

    TracePoint serialized(1u, 2u, sizes, vars, aux);
    TracePoint deserialized;

    oa & serialized;

    std::istringstream iss(oss.str());
    boost::archive::text_iarchive ia(iss);
    ia & deserialized;

    ASSERT(serialized == deserialized);
    ASSERT(hash_value(serialized) == hash_value(deserialized));
}

void test_trace_input_output ()
{
    VarValue value;
    TraceBufferSizes bufferSizes({
        TraceBufferSize(16, 4)
    });
    FlyweightTraceVarInfos vars({
        FlyweightTraceVarInfo(value, 5, 0,
                              test_types[0].getTypeFormat(),
                              test_types[0].getSize(),
                              0, 0)
    });
    Aux aux({
        1, 2, 3, 4
    });
    TracePoints test_points({
        TracePoint(0, 0, bufferSizes, vars, aux)
    });

    Trace t1(test_names, test_types, test_points);
    std::ofstream out(TRACE_FILE, std::ios::out | std::ios::binary);
    out << t1;
    out.close();

    std::ifstream in(TRACE_FILE, std::ios::out | std::ios::binary);
    Trace t2(in);
    in.close();

    ASSERT(t1 == t2);
    ASSERT(hash_value(t1) == hash_value(t2));
}

void test_trace_serialization ()
{
    std::ostringstream oss;
    boost::archive::text_oarchive oa(oss);

    Names names({
        "name1",
        "name2",
        "int*",
        "float",
    });
    TypeDescriptions types({
        TypeDescription(2u, POINTER, 4u),
        TypeDescription(3u, FLOAT, 4u)
    });
    TracePoints points;
    VarValue value;
    TraceBufferSizes sizes;
    FlyweightTraceVarInfos vars;
    Aux aux;

    value.u = 1u;
    sizes.push_back(TraceBufferSize(1u, 1u));
    vars.push_back(FlyweightTraceVarInfo(value, 1u, 1u,
                                         types[1u].getTypeFormat(),
                                         types[1u].getSize(),
                                         5u, 1u));
    aux.push_back(32u);

    points.push_back(TracePoint(1u, 1u, sizes, vars, aux));
    points.resize(512);

    Trace serialized(names, types, points);
    Trace deserialized;

    oa & serialized;

    std::istringstream iss(oss.str());
    boost::archive::text_iarchive ia(iss);
    ia & deserialized;

    ASSERT(serialized == deserialized);
    ASSERT(hash_value(serialized) == hash_value(deserialized));
}

void test_trace_db_serialization ()
{
    std::ostringstream oss;
    boost::archive::text_oarchive oa(oss);

    std::vector<Trace> traces;
    traces.resize(4);

    TraceDB serialized(traces);
    TraceDB deserialized;

    oa & serialized;

    std::istringstream iss(oss.str());
    boost::archive::text_iarchive ia(iss);
    ia & deserialized;

    ASSERT(serialized == deserialized);
    ASSERT(hash_value(serialized) == hash_value(deserialized));
}

void test_trace_point_data_equality ()
{
    TraceBufferSizes sizes;
    FlyweightTraceVarInfos vars;
    Aux aux1({1});
    Aux aux2({2});

    ASSERT(!(TracePointData(sizes, vars, aux1) ==
             TracePointData(sizes, vars, aux2)));
}

int main(int argc, char **argv)
{
    failure_count = 0;
    RUN_TEST(test_eof_in_header);
    RUN_TEST(test_good_header);
    RUN_TEST(test_bad_tag);
    RUN_TEST(test_eof_in_tag);
    RUN_TEST(test_eof_in_id);
    RUN_TEST(test_eof_in_variable);
    RUN_TEST(test_eof_in_blob_variable);
    RUN_TEST(test_bad_index_in_variable);
    RUN_TEST(test_bad_type);
    RUN_TEST(test_eof_in_buffer_size);
    RUN_TEST(test_read_from_fifo);
    RUN_TEST(test_timeout_from_fifo);
    RUN_TEST(test_memory_map);
    RUN_TEST(test_query_variable_binding);
    RUN_TEST(test_query_predicates);
    RUN_TEST(test_query_var_sizes);
    RUN_TEST(test_query_soft_predicates);
    RUN_TEST(test_trace_buffer_size_input_output);
    RUN_TEST(test_trace_buffer_size_serialization);
    RUN_TEST(test_type_description_serialization);
    RUN_TEST(test_type_description_input_output);
    RUN_TEST(test_trace_var_info_serialization);
    RUN_TEST(test_trace_var_info_input_output);
    RUN_TEST(test_trace_point_serialization);
    RUN_TEST(test_trace_input_output);
    RUN_TEST(test_trace_serialization);
    RUN_TEST(test_trace_db_serialization);
    RUN_TEST(test_trace_point_data_equality);

    unlink(TRACE_FILE);

    if (failure_count == 0) {
        printf("PASS!\n");
        return 0;
    }
    else {
        printf("%u failures\n", failure_count);
        return 1;
    }
}
