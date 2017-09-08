#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "read-trace.h"
#include "write-trace.h"

#define TRACE_FILE "test/tmp.trace"

#define N_ELTS(array) sizeof(array)/sizeof(*array)

const char *test_names[] = { "fixed_blob", "var_blob", "int", "var" };
type_description test_types[] = { {2, SIGNED, sizeof(int)},
                                  {0, BLOB, 10},
                                  {1, BLOB, 0} };

FILE *write_test_header()
{
    FILE *out = fopen(TRACE_FILE, "w");
    write_trace_header(out, test_names, N_ELTS(test_names),
                       test_types, N_ELTS(test_types));
    return out;
}

FILE *write_trace_with_variable(uint16_t name_index, uint16_t type_index)
{
    FILE *out = write_test_header();
    fputc(VARIABLE, out);
    fwrite(&name_index, sizeof(name_index), 1, out);
    fwrite(&type_index, sizeof(type_index), 1, out);

    return out;
}

void test_eof_in_header()
{
    /* Empty file */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        fclose(out);
        assert(start_reading(TRACE_FILE) == NULL);
    }

    /* Truncated name dictionary */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        uint16_t val[] = { 100, 0, 0, 0 };
        fwrite(val, sizeof(val), 1, out);
        fclose(out);

        assert(start_reading(TRACE_FILE) == NULL);
    }

    /* Missing type dictionary */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        const char *name = "foo";
        uint16_t val = strlen(name) + 1;
        fwrite(&val, sizeof(val), 1, out);
        fwrite(name, 1, strlen(name), out);
        fputc(0, out);
        fclose(out);

        assert(start_reading(TRACE_FILE) == NULL);
    }

    /* Truncated type dictionary */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        const char *name = "foo";
        uint16_t val = strlen(name) + 1;
        fwrite(&val, sizeof(val), 1, out);
        fwrite(name, 1, strlen(name), out);
        fputc(0, out);
        uint16_t vals[] = { 10, 0, 0, 0 };
        fwrite(&vals, sizeof(vals), 1, out);
        fclose(out);

        assert(start_reading(TRACE_FILE) == NULL);
    }
}

void test_good_header()
{
    FILE *out = write_test_header();
    fputc(END_ENTRY, out);
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE);
    assert(state);
    assert(state->n_names == 4);
    assert(state->n_types == 3);
    assert(read_tag(state) == END_ENTRY);

    end_reading(state);
}

void test_bad_tag()
{
    FILE *out = write_test_header();
    fputc(END_OF_TRACE + 2, out);
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE);
    assert(read_tag(state) == TRACE_TAG_ERROR);

    end_reading(state);
}

void test_eof_in_tag()
{
    FILE *out = write_test_header();
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE);
    assert(read_tag(state) == END_OF_TRACE);

    end_reading(state);
}

void test_eof_in_id()
{
    FILE *out = write_test_header();
    fputc(STATEMENT_ID, out);
    fputc(0xab, out);           /* arbitrary byte */
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE);
    assert(read_tag(state) == STATEMENT_ID);
    assert(read_id(state) == 0);
}

void test_eof_in_variable()
{
    /* EOF in name index */
    FILE *out = write_test_header();
    fputc(VARIABLE, out);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE);
    assert(read_tag(state) == VARIABLE);
    assert(read_var_info(state).size == 0);

    /* EOF in var index */
    out = write_test_header();
    fputc(VARIABLE, out);
    fputc(0, out);
    fputc(0, out);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    state = start_reading(TRACE_FILE);
    assert(read_tag(state) == VARIABLE);
    assert(read_var_info(state).size == 0);

    /* EOF in data */
    out = write_trace_with_variable(0, 3);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    state = start_reading(TRACE_FILE);
    assert(read_tag(state) == VARIABLE);
    assert(read_var_info(state).size == 0);
}

void test_eof_in_blob_variable()
{
    FILE *out;
    trace_read_state *state;

    /* EOF in fixed data */
    out = write_trace_with_variable(1, 1);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    state = start_reading(TRACE_FILE);
    assert(read_tag(state) == VARIABLE);
    assert(read_var_info(state).size == 0);

    /* EOF in size */
    out = write_trace_with_variable(2, 3);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    state = start_reading(TRACE_FILE);
    assert(read_tag(state) == VARIABLE);
    assert(read_var_info(state).size == 0);

    /* EOF in variable data */
    out = write_trace_with_variable(2, 3);
    /* data size */
    uint16_t size = 3;
    fwrite(&size, sizeof(size), 1, out);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    state = start_reading(TRACE_FILE);
    assert(read_tag(state) == VARIABLE);
    assert(read_var_info(state).size == 0);
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

    state = start_reading(TRACE_FILE);
    assert(read_tag(state) == VARIABLE);
    assert(read_var_info(state).size == 0);

    /* Bad type index */
    out = write_trace_with_variable(0, 25);
    fwrite(&value, sizeof(value), 1, out);
    fclose(out);

    state = start_reading(TRACE_FILE);
    assert(read_tag(state) == VARIABLE);
    assert(read_var_info(state).size == 0);

}

void test_bad_type()
{
    /* Invalid format */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        type_description types[] = { {2, 14, sizeof(int)} };
        write_trace_header(out, test_names, N_ELTS(test_names),
                           types, N_ELTS(types));
        fclose(out);

        assert(start_reading(TRACE_FILE) == NULL);
    }

    /* Bad name index */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        type_description types[] = { {15, SIGNED, sizeof(int)} };
        write_trace_header(out, test_names, N_ELTS(test_names),
                           types, N_ELTS(types));
        fclose(out);

        assert(start_reading(TRACE_FILE) == NULL);
    }
}

void test_eof_in_buffer_size()
{
    FILE *out = write_test_header();
    fputc(BUFFER_SIZE, out);
    fputc(0xab, out);           /* arbitrary byte */
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE);
    assert(read_tag(state) == BUFFER_SIZE);
    assert(read_buffer_size(state).address == 0);
}

int main(int argc, char **argv)
{
    test_eof_in_header();
    test_good_header();
    test_bad_tag();
    test_eof_in_tag();
    test_eof_in_id();
    test_eof_in_variable();
    test_eof_in_blob_variable();
    test_bad_index_in_variable();
    test_bad_type();
    test_eof_in_buffer_size();

    unlink(TRACE_FILE);

    return 0;
}
