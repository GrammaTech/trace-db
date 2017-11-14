/*
Sample code for reading and writing binary traces.

Usage:
sample [--write] <filename>

With --write, writes a trace to <filename> while printing "ground truth" to
stdout. Otherwise, read a trace from <filename> and print the results.

This can be used as a self-test with:
sample --write trace.out > expected && diff expected <(sample trace.out)

If reading and writing are working correctly, both invocations of sample
will produce the same output.
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "read-trace.h"
#include "write-trace.h"

void write_test_trace(const char *filename)
{
    FILE *out = fopen(filename, "w");

    const char *names[] = {
        "int", "*char", "char", "float", "double", "unsigned int", "string",
        "i", "ptr", "c", "f", "d", "u"
    };

    type_description types[] = {
        { 0, SIGNED, sizeof(int) },            /* int */
        { 1, POINTER, sizeof(char *)},         /* *char */
        { 2, SIGNED, sizeof(char) },           /* char */
        { 3, FLOAT, sizeof(float) },           /* float */
        { 4, FLOAT, sizeof(double) },          /* double */
        { 5, UNSIGNED, sizeof(unsigned int) }, /* unsigned int */
        { 6, BLOB, 0 }, /* unsigned int */
    };

    size_t n_names = sizeof(names) / sizeof(*names);
    size_t n_types = sizeof(types) / sizeof(*types);
    write_trace_header(out, names, n_names, types, n_types);

    printf("names:\n");
    for (uint32_t i = 0; i < n_names; i++) {
        printf("  %s\n", names[i]);
    }
    printf("\ntypes:\n");
    for (uint32_t i = 0; i < n_types; i++) {
        type_description type = types[i];
        printf("  %s: %u, %u bytes\n",
               names[type.name_index], type.format, type.size);
    }

    char *chars = "hello, world";
    for (int i = 0; i < 10; i++) {
        char *ptr = chars + i;
        char c = *ptr;
        float f = 0.1 * i;
        double d = 0.2 * i;
        unsigned int u = 2 * i;

        write_trace_id(out, 100 + i);
        write_trace_variables(out, 6,
                              7, 0, sizeof(i), SIGNED, i,
                              8, 1, sizeof(ptr), POINTER, ptr,
                              9, 2, sizeof(c), UNSIGNED, c,
                              10, 3, sizeof(f), FLOAT, f,
                              11, 4, sizeof(d), FLOAT, d,
                              12, 5, sizeof(u), UNSIGNED, u);
        write_trace_blobs(out, 1, 8, 6, strlen(ptr), ptr);
        /* Fake buffer size */
        write_buffer_size(out, (void *)(size_t)(0xff + i), 10 * i);
        uint64_t uint = 100 * i;
        write_trace_aux(out, uint);
        write_end_entry(out);

        printf("ID: %d\n", 100 + i);
        printf("i: int, %lu bytes = %d\n", sizeof(i), i);
        printf("ptr: *char, %lu bytes = %lx\n", sizeof(ptr), (size_t)ptr);
        printf("c: char, %lu bytes = %u '%c'\n", sizeof(c), c, c);
        printf("f: float, %lu bytes = %g\n", sizeof(f), f);
        printf("d: double, %lu bytes = %g\n", sizeof(d), d);
        printf("u: unsigned int, %lu bytes = %u\n", sizeof(u), u);
        printf("ptr: string, %lu bytes = blob: '%s'\n", strlen(ptr), ptr);
        printf("buffer size: %x . %u\n", (0xff + i), 10 * i);
        printf("uint: %lu bytes = %lu\n", sizeof(uint), uint);
        printf("\n");
    }
    printf("read 10 trace points\n");

    fclose(out);
}

void read_trace(const char *filename)
{
    trace_read_state *state = start_reading(filename, 10);
    assert(state);

    printf("names:\n");
    for (uint32_t i = 0; i < state->n_names; i++) {
        printf("  %s\n", state->names[i]);
    }
    printf("\ntypes:\n");
    for (uint32_t i = 0; i < state->n_types; i++) {
        type_description type = state->types[i];
        printf("  %s: %u, %u bytes\n",
               state->names[type.name_index], type.format, type.size);
    }

    int count = 0;
    while (1) {
        enum trace_entry_tag tag = read_tag(state);
        assert(state->error_code != TRACE_ERROR);
        if (state->error_code == TRACE_EOF)
            break;

        switch (tag) {
        case END_ENTRY:
            printf("\n");
            count++;
            break;
        case STATEMENT_ID:
            {
                uint64_t id = read_id(state);
                assert(state->error_code != TRACE_ERROR);
                printf("ID: %lu\n", id);
                break;
            }
        case VARIABLE:
            {
                trace_var_info info = read_var_info(state);
                assert(state->error_code != TRACE_ERROR);

                type_description type = state->types[info.type_index];
                printf("%s: %s, %u bytes = ", state->names[info.name_index],
                       state->names[type.name_index], info.size);
                switch (type.format) {
                case UNSIGNED:
                    printf("%lu", info.value.u);
                    if (type.size == 1)
                        printf(" '%c'", (unsigned char)info.value.u);
                    break;
                case SIGNED:
                    printf("%ld", info.value.s);
                    if (type.size == 1)
                        printf(" '%c'", (char)info.value.u);
                    break;
                case POINTER:
                    printf("%lx", info.value.u);
                    break;
                case FLOAT:
                    if (type.size == 4)
                        printf("%g", info.value.f);
                    else
                        printf("%g", info.value.d);
                    break;
                case BLOB:
                    printf("blob: '%.*s'", info.size, (const char *)info.value.ptr);
                    free(info.value.ptr);
                    break;
                default:
                    printf("<unrecognized format %u>", type.format);
                }
                printf("\n");
            }

          break;
        case BUFFER_SIZE:
            {
                trace_buffer_size info = read_buffer_size(state);
                assert(state->error_code != TRACE_ERROR);

                printf("buffer size: %lx . %lu\n", info.address, info.size);
                break;
            }
        case AUXILIARY:
            {
                uint64_t value;
                assert(fread(&value, sizeof(value), 1, state->file) == 1);

                printf("uint: %lu bytes = %lu\n", sizeof(value), value);
                break;
            }
        default:
            fprintf(stderr, "ERROR: unknown trace tag %u\n", tag);
            exit(1);
        }
    }

    end_reading(state);

    printf("read %d trace points\n", count);
}

void read_trace_2(const char *filename)
{
    trace_read_state *state = start_reading(filename, 10);
    assert(state);

    printf("names:\n");
    for (uint32_t i = 0; i < state->n_names; i++) {
        printf("  %s\n", state->names[i]);
    }
    printf("\ntypes:\n");
    for (uint32_t i = 0; i < state->n_types; i++) {
        type_description type = state->types[i];
        printf("  %s: %u, %u bytes\n",
               state->names[type.name_index], type.format, type.size);
    }

    int count = 0;
    trace_point point;

    while (read_trace_point(state, &point) == 0) {
        count++;
        printf("ID: %lu\n", point.statement);
        for (uint32_t i = 0; i < point.n_vars; i++) {
            trace_var_info info = point.vars[i];
            assert(state->error_code != TRACE_ERROR);

            type_description type = state->types[info.type_index];
            printf("%s: %s, %u bytes = ", state->names[info.name_index],
                   state->names[type.name_index], info.size);
            switch (type.format) {
            case UNSIGNED:
                printf("%lu", info.value.u);
                if (type.size == 1)
                    printf(" '%c'", (unsigned char)info.value.u);
                break;
            case SIGNED:
                printf("%ld", info.value.s);
                if (type.size == 1)
                    printf(" '%c'", (char)info.value.u);
                break;
            case POINTER:
                printf("%lx", info.value.u);
                break;
            case FLOAT:
                if (type.size == 4)
                    printf("%g", info.value.f);
                else
                    printf("%g", info.value.d);
                break;
            case BLOB:
                printf("blob: '%.*s'", info.size, (const char *)info.value.ptr);
                free(info.value.ptr);
                break;
            default:
                printf("<unrecognized format %u>", type.format);
            }
            printf("\n");
        }
        for (uint32_t i = 0; i < point.n_sizes; i++) {
            trace_buffer_size info = point.sizes[i];
            assert(state->error_code != TRACE_ERROR);

            printf("buffer size: %lx . %lu\n", info.address, info.size);
            break;
        }
        for (uint32_t i = 0; i < point.n_aux; i++) {
            uint64_t val = point.aux[i];

            printf("uint: %lu bytes = %lu\n", sizeof(val), val);
            break;
        }
        printf("\n");
    }

    end_reading(state);

    printf("read %d trace points\n", count);
}

int main(int argc, char **argv)
{
    if (!strcmp(argv[1], "--write")) {
        write_test_trace(argv[2]);
    }
    else {
        read_trace_2(argv[1]);
    }

    return 0;
}
