#include <cassert>
#include <cfloat>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sstream>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>

#include "read-trace.h"
#include "trace-db.h"
#include "write-trace.h"

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

const char *test_names[] = { "fixed_blob", "var_blob", "int", "var",
                             "x", "y", "z", "unsigned int",
                             "big_positive", "big_negative",
                             "int64_t", "uint64_t", "big_unsigned",
                             "*void"};
type_description test_types[] = { {2, SIGNED, sizeof(int)},
                                  {0, BLOB, 10},
                                  {1, BLOB, 0},
                                  {7, UNSIGNED, sizeof(unsigned int)},
                                  {10, SIGNED, sizeof(int64_t)},
                                  {11, UNSIGNED, sizeof(uint64_t)},
                                  {13, POINTER, sizeof(void *)}};

FILE *write_test_header()
{
    FILE *out = fopen(TRACE_FILE, "w");
    write_trace_header(out, test_names, N_ELTS(test_names),
                       test_types, N_ELTS(test_types));
    return out;
}

FILE *write_trace_with_variable(uint32_t name_index, uint32_t type_index)
{
    FILE *out = write_test_header();
    fputc(VARIABLE, out);
    fwrite(&name_index, sizeof(name_index), 1, out);
    fwrite(&type_index, sizeof(type_index), 1, out);

    return out;
}

uint16_t namei(const char *name)
{
    for (unsigned i = 0; i < N_ELTS(test_names); i++) {
        if (!strcmp(name, test_names[i]))
            return i;
    }
    assert(0);
}

uint16_t typei(const char *name)
{
    for (unsigned i = 0; i < N_ELTS(test_types); i++) {
        if (!strcmp(name, test_names[test_types[i].name_index]))
            return i;
    }
    assert(0);
}

void test_eof_in_header()
{
    /* Empty file */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        fclose(out);
        ASSERT(start_reading(TRACE_FILE, TIMEOUT) == NULL);
    }

    /* Truncated name dictionary */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        uint16_t val[] = { 100, 0, 0, 0 };
        fwrite(val, sizeof(val), 1, out);
        fclose(out);

        ASSERT(start_reading(TRACE_FILE, TIMEOUT) == NULL);
    }

    /* Missing type dictionary */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        const char *name = "foo";
        uint64_t val = strlen(name) + 1;
        fwrite(&val, sizeof(val), 1, out);
        fwrite(name, 1, strlen(name), out);
        fputc(0, out);
        fclose(out);

        ASSERT(start_reading(TRACE_FILE, TIMEOUT) == NULL);
    }

    /* Truncated type dictionary */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        const char *name = "foo";
        uint64_t val = strlen(name) + 1;
        fwrite(&val, sizeof(val), 1, out);
        fwrite(name, 1, strlen(name), out);
        fputc(0, out);
        uint16_t vals[] = { 10, 0, 0, 0 };
        fwrite(&vals, sizeof(vals), 1, out);
        fclose(out);

        ASSERT(start_reading(TRACE_FILE, TIMEOUT) == NULL);
    }
}

void test_good_header()
{
    FILE *out = write_test_header();
    fputc(END_ENTRY, out);
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(state);
    ASSERT(state->n_names == N_ELTS(test_names));
    ASSERT(state->n_types == N_ELTS(test_types));
    ASSERT(read_tag(state) == END_ENTRY);
    ASSERT(state->error_code == TRACE_OK);

    end_reading(state);
}

void test_bad_tag()
{
    FILE *out = write_test_header();
    fputc(INVALID_TAG + 2, out);
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(state->error_code == TRACE_OK);
    read_tag(state);
    ASSERT(state->error_code == TRACE_ERROR);

    end_reading(state);
}

void test_eof_in_tag()
{
    FILE *out = write_test_header();
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(state->error_code == TRACE_OK);
    read_tag(state);
    ASSERT(state->error_code == TRACE_EOF);

    end_reading(state);
}

void test_eof_in_id()
{
    FILE *out = write_test_header();
    fputc(STATEMENT_ID, out);
    fputc(0xab, out);           /* arbitrary byte */
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(read_tag(state) == STATEMENT_ID);
    ASSERT(state->error_code == TRACE_OK);
    read_id(state);
    ASSERT(state->error_code == TRACE_EOF);
}

void test_eof_in_variable()
{
    /* EOF in name index */
    FILE *out = write_test_header();
    fputc(VARIABLE, out);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(read_tag(state) == VARIABLE);
    ASSERT(state->error_code == TRACE_OK);
    read_var_info(state);
    ASSERT(state->error_code == TRACE_EOF);

    /* EOF in var index */
    out = write_test_header();
    fputc(VARIABLE, out);
    fputc(0, out);
    fputc(0, out);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(read_tag(state) == VARIABLE);
    ASSERT(state->error_code == TRACE_OK);
    read_var_info(state);
    ASSERT(state->error_code == TRACE_EOF);

    /* EOF in data */
    out = write_trace_with_variable(0, 0);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(read_tag(state) == VARIABLE);
    ASSERT(state->error_code == TRACE_OK);
    read_var_info(state);
    ASSERT(state->error_code == TRACE_EOF);
}

void test_eof_in_blob_variable()
{
    FILE *out;
    trace_read_state *state;

    /* EOF in fixed data */
    out = write_trace_with_variable(1, 1);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(read_tag(state) == VARIABLE);
    ASSERT(state->error_code == TRACE_OK);
    read_var_info(state);
    ASSERT(state->error_code == TRACE_EOF);

    /* EOF in size */
    out = write_trace_with_variable(2, 2);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(read_tag(state) == VARIABLE);
    ASSERT(state->error_code == TRACE_OK);
    read_var_info(state);
    ASSERT(state->error_code == TRACE_EOF);

    /* EOF in variable data */
    out = write_trace_with_variable(2, 2);
    /* data size */
    uint16_t size = 3;
    fwrite(&size, sizeof(size), 1, out);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(read_tag(state) == VARIABLE);
    ASSERT(state->error_code == TRACE_OK);
    read_var_info(state);
    ASSERT(state->error_code == TRACE_EOF);
}


void test_bad_index_in_variable()
{
    FILE *out;
    trace_read_state *state;

    /* Bad name index */
    out = write_trace_with_variable(25, 0);
    int value = 1234;
    fwrite(&value, sizeof(value), 1, out);
    fclose(out);

    state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(read_tag(state) == VARIABLE);
    ASSERT(state->error_code == TRACE_OK);
    read_var_info(state);
    ASSERT(state->error_code == TRACE_ERROR);

    /* Bad type index */
    out = write_trace_with_variable(0, N_ELTS(test_types));
    fwrite(&value, sizeof(value), 1, out);
    fclose(out);

    state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(read_tag(state) == VARIABLE);
    ASSERT(state->error_code == TRACE_OK);
    read_var_info(state);
    ASSERT(state->error_code == TRACE_ERROR);
}

void test_bad_type()
{
    /* Invalid format */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        type_description types[1];

        types[0].name_index = 2;
        types[0].format = (type_format) 14;
        types[0].size = sizeof(int);
        write_trace_header(out, test_names, N_ELTS(test_names),
                           types, N_ELTS(types));
        fclose(out);

        ASSERT(start_reading(TRACE_FILE, TIMEOUT) == NULL);
    }

    /* Bad name index */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        type_description types[] = { {15, SIGNED, sizeof(int)} };
        write_trace_header(out, test_names, N_ELTS(test_names),
                           types, N_ELTS(types));
        fclose(out);

        ASSERT(start_reading(TRACE_FILE, TIMEOUT) == NULL);
    }
}

void test_eof_in_buffer_size()
{
    FILE *out = write_test_header();
    fputc(BUFFER_SIZE, out);
    fputc(0xab, out);           /* arbitrary byte */
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE, TIMEOUT);
    ASSERT(read_tag(state) == BUFFER_SIZE);
    ASSERT(state->error_code == TRACE_OK);
    read_buffer_size(state);
    ASSERT(state->error_code == TRACE_EOF);
}

void test_read_from_fifo()
{
    FILE *out = write_test_header();
    fclose(out);

    mkfifo("test/fifo", 0700);
    system("cat test/tmp.trace > test/fifo &");

    trace_read_state *state = start_reading("test/fifo", TIMEOUT);
    unlink("test/fifo");

    ASSERT(state);
    read_tag(state);
    ASSERT(state->error_code == TRACE_EOF);
    end_reading(state);

}

void test_timeout_from_fifo()
{
    mkfifo("test/fifo", 0700);

    trace_read_state *state = start_reading("test/fifo", 1);
    unlink("test/fifo");

    assert(state == NULL);
}

void test_memory_map()
{
    type_description type = {0, POINTER, 8};
    trace_read_state state;
    trace_point point;
    trace_var_info var;
    skip_list *memory_map = create_memory_map();
    trace_buffer_size sizes[] = {{ 3, 10 }, { 100, 2}};

    state.n_types = 1;
    state.types = &type;
    point.n_sizes = 2;
    point.sizes = sizes;
    var.type_index = 0;
    update_memory_map(memory_map, &point);

    /* Basic lookups */
    var.value.ptr = (void *)3;
    compute_buffer_size(memory_map, &state, &var);
    ASSERT(var.buffer_size == 10);

    var.value.ptr = (void *)100;
    compute_buffer_size(memory_map, &state, &var);
    ASSERT(var.buffer_size == 2);

    /* Pointers in the middle of the buffer */
    var.value.ptr = (void *)8;
    compute_buffer_size(memory_map, &state, &var);
    ASSERT(var.buffer_size == 5);

    var.value.ptr = (void *)101;
    compute_buffer_size(memory_map, &state, &var);
    ASSERT(var.buffer_size == 1);

    /* End of buffer */
    var.value.ptr = (void *)13;
    compute_buffer_size(memory_map, &state, &var);
    ASSERT(var.has_buffer_size == 0);

    var.value.ptr = (void *)102;
    compute_buffer_size(memory_map, &state, &var);
    ASSERT(var.has_buffer_size == 0);

    /* Pointers outside known buffers */
    var.value.ptr = (void *)99;
    compute_buffer_size(memory_map, &state, &var);
    ASSERT(var.has_buffer_size == 0);

    var.value.ptr = (void *)2;
    compute_buffer_size(memory_map, &state, &var);
    ASSERT(var.has_buffer_size == 0);

    /* New allocation supersedes old */
    sizes[0] = (trace_buffer_size) {3, 5};
    update_memory_map(memory_map, &point);
    var.value.ptr = (void *)3;
    compute_buffer_size(memory_map, &state, &var);
    ASSERT(var.buffer_size == 5);

    /* Freed memory is removed from the map */
    sizes[1] = (trace_buffer_size) {100, 0};
    update_memory_map(memory_map, &point);
    var.value.ptr = (void *)100;
    compute_buffer_size(memory_map, &state, &var);
    ASSERT(var.has_buffer_size == 0);

    /* UINT64_MAX is never found in map (used as a sentinel) */
    var.value.ptr = (void *)UINT64_MAX;
    compute_buffer_size(memory_map, &state, &var);
    ASSERT(var.has_buffer_size == 0);

    /* Inserting UINT64_MAX does nothing */
    sizes[0] = (trace_buffer_size) {UINT64_MAX, 5};
    update_memory_map(memory_map, &point);
    var.value.ptr = (void *)UINT64_MAX;
    compute_buffer_size(memory_map, &state, &var);
    ASSERT(var.has_buffer_size == 0);

    /* Free works */
    free_memory_map(memory_map);
}

trace_db *create_test_db()
{
    FILE *out = write_test_header();
    write_trace_id(out, 1);
    unsigned int x = 0, y = 1;
    int z = -2;
    int64_t big_positive = INT64_MAX;
    int64_t big_negative = INT64_MIN;
    uint64_t big_unsigned = UINT64_MAX;
    void *voidptr = (void*)0x1234;
    write_trace_variables(out, 7,
                          namei("x"), typei("unsigned int"),
                          sizeof(x), UNSIGNED, x,
                          namei("y"), typei("unsigned int"),
                          sizeof(y), UNSIGNED, y,
                          namei("z"), typei("int"),
                          sizeof(z), SIGNED, z,
                          namei("big_positive"), typei("int64_t"),
                          sizeof(big_positive), SIGNED, big_positive,
                          namei("big_negative"), typei("int64_t"),
                          sizeof(big_negative), SIGNED, big_negative,
                          namei("big_unsigned"), typei("uint64_t"),
                          sizeof(big_unsigned), SIGNED, big_unsigned,
                          namei("var"), typei("*void"),
                          sizeof(voidptr), POINTER, voidptr);
    write_buffer_size(out, voidptr, 4);
    write_end_entry(out);
    fclose(out);

    trace_db *db = create_db();
    add_trace(db, start_reading(TRACE_FILE, TIMEOUT), 0);

    return db;
}

void test_query_variable_binding()
{
    trace_db *db = create_test_db();

    trace_point *results;
    uint64_t n_results;
    uint32_t type_a = typei("unsigned int");
    uint32_t type_b = typei("int");
    free_variable vars[] = { { 1, &type_a }, { 1, &type_b } };
    query_trace(db, 0, N_ELTS(vars), vars,
                NULL, NULL, 0, 0, 0, 0,
                &results, &n_results);

    ASSERT(n_results == 2);
    ASSERT(results[0].n_vars == 2);
    ASSERT(results[0].vars[0].name_index = namei("x"));
    ASSERT(results[0].vars[0].value.u == 0);
    ASSERT(results[0].vars[1].name_index == namei("z"));
    ASSERT(results[0].vars[1].value.s == -2);

    ASSERT(results[1].n_vars == 2);
    ASSERT(results[1].vars[0].name_index = namei("y"));
    ASSERT(results[1].vars[0].value.u == 1);
    ASSERT(results[1].vars[1].name_index == namei("z"));
    ASSERT(results[1].vars[1].value.s == -2);

    free_query_result(results, n_results);
    free_db(db);
}

void test_query_predicates()
{
    trace_db *db = create_test_db();

    trace_point *results;
    uint64_t n_results;
    uint32_t type_a = typei("unsigned int");
    uint32_t type_b = typei("int");
    uint32_t type_i64 = typei("int64_t");

    predicate var1 = { VAR_REFERENCE, {0} };
    predicate var2 = { VAR_REFERENCE, {1} };
    predicate var1_val = { VAR_VALUE, {1}, &var1 };
    predicate var2_val = { VAR_VALUE, {1}, &var2 };
    predicate vars[] = { var1, var2 };
    predicate var_values[] = { var1_val, var2_val };
    predicate distinct_vars = { DISTINCT_VARS, {2}, vars };

    uint32_t int_types[] = { type_a, type_b };
    free_variable int_vars[] = { { 2, int_types }, { 2, int_types } };


    predicate negative_two;
    negative_two.kind = SIGNED_INTEGER;
    negative_two.data.signed_value = -2;

    predicate negative_one;
    negative_one.kind = SIGNED_INTEGER;
    negative_one.data.signed_value = -1;

    predicate zero;
    zero.kind = SIGNED_INTEGER;
    zero.data.signed_value = 0;

    predicate three;
    three.kind = SIGNED_INTEGER;
    three.data.signed_value = 3;

    predicate int64_max;
    int64_max.kind = SIGNED_INTEGER;
    int64_max.data.signed_value = INT64_MAX;

    predicate uint64_max;
    uint64_max.kind = UNSIGNED_INTEGER;
    uint64_max.data.signed_value = UINT64_MAX;

    predicate int64_min;
    int64_min.kind = SIGNED_INTEGER;
    int64_min.data.signed_value = INT64_MIN;


    /* Distinct variables */
    {
        free_variable vars[] = { { 1, &type_a }, { 1, &type_a } };

        /* Unrestricted query should return 4 results, two bindings for each
         * variable */
        query_trace(db, 0, N_ELTS(vars), vars,
                    NULL, NULL, 0, 0, 0, 0,
                    &results, &n_results);

        ASSERT(n_results == 4);
        free_query_result(results, n_results);

        /* With DISTINCT_VARS predicate, only two results are valid */
        query_trace(db, 0, N_ELTS(vars), vars,
                    &distinct_vars, NULL, 0, 0, 0, 0,
                    &results, &n_results);

        ASSERT(n_results == 2);
        free_query_result(results, n_results);
    }

    /* Greater than, unsigned constant */
    {
        free_variable vars[] = { { 2, int_types } };

        /* a > 0 */
        predicate p0[] = { var1_val, zero };
        predicate p = { GREATER_THAN, {2}, p0 };

        query_trace(db, 0, N_ELTS(vars), vars,
                    &p, NULL, 0, 0, 0, 0,
                    &results, &n_results);
        ASSERT(n_results == 1);
        free_query_result(results, n_results);
    }

    /* Less than, signed constant */
    {
        free_variable vars[] = { { 2, int_types } };

        /* a < -1 */
        predicate p0[] = { var1_val, negative_one };
        predicate p = { LESS_THAN, {2}, p0 };

        query_trace(db, 0, N_ELTS(vars), vars,
                    &p, NULL, 0, 0, 0, 0,
                    &results, &n_results);
        ASSERT(n_results == 1);
        free_query_result(results, n_results);
    }

    /* Addition */
    {
        /* a + b == -1 */
        predicate p0[] = { { ADD, {2}, var_values }, negative_one };
        predicate p = { EQUAL, {2}, p0 };

        query_trace(db, 0, N_ELTS(int_vars), int_vars,
                    &p, NULL, 0, 0, 0, 0,
                    &results, &n_results);
        ASSERT(n_results == 2);
        ASSERT(results[0].vars[0].name_index == namei("z"));
        ASSERT(results[0].vars[1].name_index == namei("y"));
        free_query_result(results, n_results);
    }

    /* Subtraction */
    {
        /* a - b == 3 */
        predicate p0[] = { { SUBTRACT, {2}, var_values }, three };
        predicate p = { EQUAL, {2}, p0 };

        query_trace(db, 0, N_ELTS(int_vars), int_vars,
                    &p, NULL, 0, 0, 0, 0,
                    &results, &n_results);
        ASSERT(n_results == 1);
        ASSERT(results[0].vars[0].name_index == namei("y"));
        ASSERT(results[0].vars[1].name_index == namei("z"));
        free_query_result(results, n_results);
    }

    /* Multiplication */
    {
        predicate zero;
        zero.kind = SIGNED_INTEGER;
        zero.data.signed_value = 0;

        /* a * b == 0 */
        predicate p0[] = { { MULTIPLY, {2}, var_values }, zero };
        predicate p = { EQUAL, {2}, p0 };

        query_trace(db, 0, N_ELTS(int_vars), int_vars,
                    &p, NULL, 0, 0, 0, 0,
                    &results, &n_results);
        ASSERT(n_results == 5);
        for (int i = 0; i < 5; i++)
            ASSERT(results[i].vars[0].name_index == namei("x") ||
                   results[i].vars[1].name_index == namei("x"));
        free_query_result(results, n_results);
    }

    /* Division */
    {
        /* a / b == 0 */
        predicate p0[] = { { DIVIDE, {2}, var_values }, zero };
        predicate p = { EQUAL, {2}, p0 };

        query_trace(db, 0, N_ELTS(int_vars), int_vars,
                    &p, NULL, 0, 0, 0, 0,
                    &results, &n_results);
        ASSERT(n_results == 3);
        ASSERT(results[0].vars[0].name_index == namei("x"));
        free_query_result(results, n_results);
    }

    /* And, or */
    {
        free_variable vars[] = { { 2, int_types } };

        /* a > -0 */
        predicate p0[] = { var1_val, zero };
        predicate gt = { GREATER_THAN, {2}, p0 };

        /* a < 3 */
        predicate p1[] = { var1_val, three };
        predicate lt = { LESS_THAN, {2}, p1 };

        /* a == -2 */
        predicate p2[] = { var1_val, negative_two };
        predicate eq = { EQUAL, {2}, p2 };

        predicate p3[] = { gt, lt };
        predicate pand = { AND, {2}, p3 };

        predicate p4[] = { pand, eq };
        predicate por = { OR, {2}, p4 };

        query_trace(db, 0, N_ELTS(vars), vars,
                    &por, NULL, 0, 0, 0, 0,
                    &results, &n_results);
        ASSERT(n_results == 2);
        ASSERT(results[0].vars[0].name_index == namei("y"));
        ASSERT(results[1].vars[0].name_index == namei("z"));
        free_query_result(results, n_results);
    }

    /* Overflow check */
    {
        /* a + b > INT64_MAX */
        predicate p0[] = { { ADD, {2}, var_values }, int64_max };
        predicate p = { GREATER_THAN, {2}, p0 };
        free_variable vars[] = { { 1, &type_i64 }, { 1, &type_i64 } };

        query_trace(db, 0, N_ELTS(vars), vars,
                    &p, NULL, 0, 0, 0, 0,
                    &results, &n_results);
        ASSERT(n_results == 1);
        ASSERT(results[0].vars[0].name_index == namei("big_positive"));
        free_query_result(results, n_results);
    }

    /* Underflow check */
    {
        /* a + b < INT64_MIN */
        predicate p0[] = { { ADD, {2}, var_values }, int64_min };
        predicate p = { LESS_THAN, {2}, p0 };
        free_variable vars[] = { { 1, &type_i64 }, { 1, &type_i64 } };

        query_trace(db, 0, N_ELTS(vars), vars,
                    &p, NULL, 0, 0, 0, 0,
                    &results, &n_results);
        ASSERT(n_results == 1);
        ASSERT(results[0].vars[0].name_index == namei("big_negative"));
        free_query_result(results, n_results);
    }

    /* Signed overflow check */
    {
        /* a + b > UINT64_MAX */
        predicate p0[] = { { ADD, {2}, var_values }, uint64_max };
        predicate p = { GREATER_THAN, {2}, p0 };
        uint32_t type_u64 = typei("uint64_t");
        free_variable vars[] = { { 1, &type_u64 }, { 1, &type_u64 } };

        query_trace(db, 0, N_ELTS(vars), vars,
                    &p, NULL, 0, 0, 0, 0,
                    &results, &n_results);
        ASSERT(n_results == 1);
        ASSERT(results[0].vars[0].name_index == namei("big_unsigned"));
        free_query_result(results, n_results);
    }

    free_db(db);
}

void test_query_var_sizes()
{
    trace_db *db = create_test_db();

    trace_point *results;
    uint64_t n_results;
    uint32_t type_a = typei("*void");

    predicate var = { VAR_REFERENCE, {0} };

    free_variable vars[] = { { 1, &type_a } };

    predicate four;
    four.kind = UNSIGNED_INTEGER;
    four.data.unsigned_value = 4;

    predicate five;
    five.kind = UNSIGNED_INTEGER;
    five.data.unsigned_value = 5;

    /* Unrestricted query should return 1 result */
    {
        query_trace(db, 0, N_ELTS(vars), vars,
                    NULL, NULL, 0, 0, 0, 0,
                    &results, &n_results);

        ASSERT(n_results == 1);
        free_query_result(results, n_results);
    }

    /* size == 4, one result */
    {
        predicate p0[] = { { VAR_SIZE, {1}, &var }, four };
        predicate p = { EQUAL, {2}, p0 };
        query_trace(db, 0, N_ELTS(vars), vars,
                    &p, NULL, 0, 0, 0, 0,
                    &results, &n_results);

        ASSERT(n_results == 1);
        free_query_result(results, n_results);
    }

    /* size == 5, no results */
    {
        predicate p0[] = { { VAR_SIZE, {1}, &var }, five };
        predicate p = { EQUAL, {2}, p0 };
        query_trace(db, 0, N_ELTS(vars), vars,
                    &p, NULL, 0, 0, 0, 0,
                    &results, &n_results);

        ASSERT(n_results == 0);
        free_query_result(results, n_results);
    }

    free_db(db);
}

void test_query_soft_predicates()
{
    trace_db *db = create_test_db();

    trace_point *results;
    uint64_t n_results;
    uint32_t types[] = { typei("int"), typei("unsigned int") };


    free_variable vars[] = { { 2, types }, { 2, types } };
    predicate var1 = { VAR_REFERENCE, {0} };
    predicate var1_val = { VAR_VALUE, {1}, &var1 };

    predicate negative_two;
    negative_two.kind = SIGNED_INTEGER;
    negative_two.data.signed_value = -2;

    predicate one;
    one.kind = SIGNED_INTEGER;
    one.data.signed_value = 1;

    /* Unrestricted: 9 matches */
    {
        query_trace(db, 0, N_ELTS(vars), vars,
                    NULL, NULL, 0, 0, 0, 0,
                    &results, &n_results);

        ASSERT(n_results == 9);
        free_query_result(results, n_results);
    }

    /* Single predicate: x and z both satisfy it */
    {
        predicate p0[] = { var1_val, one };
        predicate p = { LESS_THAN, {2}, p0 };
        const predicate *soft[] = { &p };

        query_trace(db, 0, N_ELTS(vars), vars,
                    NULL, soft, 1, 0, 0, 0,
                    &results, &n_results);

        ASSERT(n_results == 6);
        for (unsigned int i = 0; i < n_results; i++)
            ASSERT(results[i].vars[0].name_index == namei("z") ||
                   results[i].vars[0].name_index == namei("x"));
        free_query_result(results, n_results);
    }

    /* Two predicates: only z satisfies both */
    {
        predicate p0[] = { var1_val, one };
        predicate s0 = { LESS_THAN, {2}, p0 };
        predicate p1[] = { var1_val, negative_two };
        predicate s1 = { EQUAL, {2}, p1 };
        const predicate *soft[] = { &s0, &s1 };

        query_trace(db, 0, N_ELTS(vars), vars,
                    NULL, soft, 2, 0, 0, 0,
                    &results, &n_results);

        ASSERT(n_results == 3);
        for (unsigned int i = 0; i < n_results; i++)
            ASSERT(results[i].vars[0].name_index == namei("z"));
        free_query_result(results, n_results);
    }

    free_db(db);
}

void test_trace_buffer_size_serialization ()
{
    std::ostringstream oss;
    boost::archive::text_oarchive oa(oss);

    trace_buffer_size serialized;
    trace_buffer_size deserialized;

    serialized.address = 0x1000;
    serialized.size = 10;
    oa & serialized;

    std::istringstream iss(oss.str());
    boost::archive::text_iarchive ia(iss);

    ia & deserialized;

    ASSERT(serialized == deserialized);
}

void test_type_description_serialization ()
{
    std::ostringstream oss;
    boost::archive::text_oarchive oa(oss);

    type_description serialized;
    type_description deserialized;

    serialized.name_index = 1;
    serialized.format = UNSIGNED;
    serialized.size = 2;
    oa & serialized;

    std::istringstream iss(oss.str());
    boost::archive::text_iarchive ia(iss);

    ia & deserialized;

    ASSERT(serialized == deserialized);
}

void test_trace_var_info_serialization ()
{
    std::ostringstream oss;
    boost::archive::text_oarchive oa(oss);

    trace_var_info serialized1;
    trace_var_info serialized2;
    trace_var_info serialized3;
    trace_var_info serialized4;
    trace_var_info serialized5;
    trace_var_info deserialized1;
    trace_var_info deserialized2;
    trace_var_info deserialized3;
    trace_var_info deserialized4;
    trace_var_info deserialized5;

    serialized1.value.u = UINT32_MAX;
    serialized1.name_index = 2u;
    serialized1.type_index = 3u;
    serialized1.size = 4u;
    serialized1.buffer_size = 5u;
    serialized1.has_buffer_size = true;

    serialized2 = serialized1;
    serialized2.value.s = INT32_MAX;

    serialized3 = serialized1;
    serialized3.value.f = FLT_MIN;

    serialized4 = serialized1;
    serialized4.value.d = DBL_MIN;

    serialized5 = serialized1;
    serialized5.value.ptr = &serialized1;

    oa & serialized1 & serialized2 & serialized3
       & serialized4 & serialized5;

    std::istringstream iss(oss.str());
    boost::archive::text_iarchive ia(iss);
    ia & deserialized1 & deserialized2 & deserialized3
       & deserialized4 & deserialized5;

    ASSERT(serialized1 == deserialized1);
    ASSERT(serialized2 == deserialized2);
    ASSERT(serialized3 == deserialized3);
    ASSERT(serialized4 == deserialized4);
    ASSERT(serialized5 == deserialized5);
    ASSERT(deserialized1.value.u == UINT32_MAX);
    ASSERT(deserialized2.value.s == INT32_MAX);
    ASSERT(deserialized3.value.f == FLT_MIN);
    ASSERT(deserialized4.value.d == DBL_MIN);
    ASSERT(deserialized5.value.ptr == &serialized1);
}

void test_trace_point_serialization ()
{
    std::ostringstream oss;
    boost::archive::text_oarchive oa(oss);

    trace_point serialized;
    trace_point deserialized;

    serialized.statement = 1u;
    serialized.n_sizes = 1u;
    serialized.n_vars = 1u;
    serialized.n_aux = 1u;
    serialized.sizes = (trace_buffer_size*)malloc(sizeof(trace_buffer_size));
    serialized.vars = (trace_var_info*)malloc(sizeof(trace_var_info));
    serialized.aux = (uint64_t*)malloc(sizeof(uint64_t));

    serialized.sizes[0].address = 1u;
    serialized.sizes[0].size = 2u;
    serialized.vars[0].value.u = 1u;
    serialized.vars[0].name_index = 2u;
    serialized.vars[0].type_index = 3u;
    serialized.vars[0].size = 4u;
    serialized.vars[0].buffer_size = 5u;
    serialized.vars[0].has_buffer_size = true;
    serialized.aux[0] = 32u;

    oa & serialized;

    std::istringstream iss(oss.str());
    boost::archive::text_iarchive ia(iss);
    ia & deserialized;

    ASSERT(serialized == deserialized);
}

void test_trace_serialization ()
{
    std::ostringstream oss;
    boost::archive::text_oarchive oa(oss);

    trace serialized;
    trace deserialized;

    serialized.n_points = 512;
    serialized.n_points_allocated = 1024;
    serialized.points = (trace_point*) calloc(1024, sizeof(trace_point));
    serialized.points[0].statement = 1u;
    serialized.points[0].n_vars = 1u;
    serialized.points[0].vars = (trace_var_info*)malloc(sizeof(trace_var_info));
    serialized.points[0].vars[0].value.u = 1u;
    serialized.points[0].vars[0].name_index = 1u;
    serialized.points[0].vars[0].type_index = 1u;
    serialized.points[0].vars[0].size = 4u;
    serialized.points[0].vars[0].buffer_size = 5u;
    serialized.points[0].vars[0].has_buffer_size = true;

    serialized.n_names = 2;
    serialized.names = (char**) calloc(2, sizeof(char*));
    serialized.names[0] = (char*) "hello";
    serialized.names[1] = (char*) "world";

    serialized.n_types = 2;
    serialized.types = (type_description *) calloc(2, sizeof(type_description));
    serialized.types[0].name_index = 0;
    serialized.types[0].format = POINTER;
    serialized.types[0].size = 4;
    serialized.types[1].name_index = 1;
    serialized.types[1].format = FLOAT;
    serialized.types[1].size = 4;

    oa & serialized;

    std::istringstream iss(oss.str());
    boost::archive::text_iarchive ia(iss);
    ia & deserialized;

    ASSERT(serialized == deserialized);
}

void test_trace_db_serialization ()
{
    std::ostringstream oss;
    boost::archive::text_oarchive oa(oss);

    trace_db serialized;
    trace_db deserialized;

    serialized.n_traces = 4;
    serialized.n_traces_allocated = 8;
    serialized.traces = (trace*) calloc(8, sizeof(trace));
    memset(serialized.traces, 0, serialized.n_traces_allocated * sizeof(trace));

    oa & serialized;

    std::istringstream iss(oss.str());
    boost::archive::text_iarchive ia(iss);
    ia & deserialized;

    ASSERT(serialized == deserialized);
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
    RUN_TEST(test_trace_buffer_size_serialization);
    RUN_TEST(test_type_description_serialization);
    RUN_TEST(test_trace_var_info_serialization);
    RUN_TEST(test_trace_point_serialization);
    RUN_TEST(test_trace_serialization);
    RUN_TEST(test_trace_db_serialization);

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
