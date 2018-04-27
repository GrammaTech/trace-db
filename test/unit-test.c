#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "read-trace.h"
#include "trace-db.h"
#include "write-trace.h"

#define TRACE_FILE "test/tmp.trace"

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

FILE *write_trace_with_variable(uint32_t name_index, uint32_t type_index)
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
        ASSERT(start_reading(TRACE_FILE) == NULL);
    }

    /* Truncated name dictionary */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        uint16_t val[] = { 100, 0, 0, 0 };
        fwrite(val, sizeof(val), 1, out);
        fclose(out);

        ASSERT(start_reading(TRACE_FILE) == NULL);
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

        ASSERT(start_reading(TRACE_FILE) == NULL);
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

        ASSERT(start_reading(TRACE_FILE) == NULL);
    }
}

void test_good_header()
{
    FILE *out = write_test_header();
    fputc(END_ENTRY, out);
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE);
    ASSERT(state);
    ASSERT(state->n_names == 4);
    ASSERT(state->n_types == 3);
    ASSERT(read_tag(state) == END_ENTRY);
    ASSERT(state->error_code == TRACE_OK);

    end_reading(state);
}

void test_bad_tag()
{
    FILE *out = write_test_header();
    fputc(INVALID_TAG + 2, out);
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE);
    ASSERT(state->error_code == TRACE_OK);
    read_tag(state);
    ASSERT(state->error_code == TRACE_ERROR);

    end_reading(state);
}

void test_eof_in_tag()
{
    FILE *out = write_test_header();
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE);
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

    trace_read_state *state = start_reading(TRACE_FILE);
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

    trace_read_state *state = start_reading(TRACE_FILE);
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

    state = start_reading(TRACE_FILE);
    ASSERT(read_tag(state) == VARIABLE);
    ASSERT(state->error_code == TRACE_OK);
    read_var_info(state);
    ASSERT(state->error_code == TRACE_EOF);

    /* EOF in data */
    out = write_trace_with_variable(0, 0);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    state = start_reading(TRACE_FILE);
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

    state = start_reading(TRACE_FILE);
    ASSERT(read_tag(state) == VARIABLE);
    ASSERT(state->error_code == TRACE_OK);
    read_var_info(state);
    ASSERT(state->error_code == TRACE_EOF);

    /* EOF in size */
    out = write_trace_with_variable(2, 2);
    fputc(0xab, out);            /* arbitrary byte */
    fclose(out);

    state = start_reading(TRACE_FILE);
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

    state = start_reading(TRACE_FILE);
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

    state = start_reading(TRACE_FILE);
    ASSERT(read_tag(state) == VARIABLE);
    ASSERT(state->error_code == TRACE_OK);
    read_var_info(state);
    ASSERT(state->error_code == TRACE_ERROR);

    /* Bad type index */
    out = write_trace_with_variable(0, 3);
    fwrite(&value, sizeof(value), 1, out);
    fclose(out);

    state = start_reading(TRACE_FILE);
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
        type_description types[] = { {2, 14, sizeof(int)} };
        write_trace_header(out, test_names, N_ELTS(test_names),
                           types, N_ELTS(types));
        fclose(out);

        ASSERT(start_reading(TRACE_FILE) == NULL);
    }

    /* Bad name index */
    {
        FILE *out = fopen(TRACE_FILE, "w");
        type_description types[] = { {15, SIGNED, sizeof(int)} };
        write_trace_header(out, test_names, N_ELTS(test_names),
                           types, N_ELTS(types));
        fclose(out);

        ASSERT(start_reading(TRACE_FILE) == NULL);
    }
}

void test_eof_in_buffer_size()
{
    FILE *out = write_test_header();
    fputc(BUFFER_SIZE, out);
    fputc(0xab, out);           /* arbitrary byte */
    fclose(out);

    trace_read_state *state = start_reading(TRACE_FILE);
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

    trace_read_state *state = start_reading("test/fifo");
    unlink("test/fifo");

    ASSERT(state);
    read_tag(state);
    ASSERT(state->error_code == TRACE_EOF);
    end_reading(state);

}

void test_memory_map()
{
    type_description type = {0, POINTER, 8};
    trace_read_state state = { .n_types = 1,
                               .types = &type };
    skip_list *memory_map = create_memory_map();
    trace_buffer_size sizes[] = {{ 3, 10 }, { 100, 2}};
    trace_point point = { .n_sizes = 2, .sizes = sizes };
    update_memory_map(memory_map, &point);

    /* Basic lookups */
    trace_var_info var = { .type_index = 0,
                           .value = { .ptr = (void *)3 }};
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
    RUN_TEST(test_memory_map);

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
