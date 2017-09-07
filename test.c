/* Sample code for reading and writing binary traces. */

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

    write_trace_header(out, names, sizeof(names) / sizeof(*names),
                       types, sizeof(types) / sizeof(*types));

    char *chars = "hello, world";
    for (int i = 0; i < 10; i++) {
        char *ptr = chars + i;
        char c = *ptr;
        float f = 0.1 * i;
        double d = 0.2 * i;
        unsigned int u = 2 * i;

        write_trace_id(out, 100 + i);
        WRITE_TRACE_VARIABLE(out, 7, 0, i);
        WRITE_TRACE_VARIABLE(out, 8, 1, ptr);
        WRITE_TRACE_VARIABLE(out, 9, 2, c);
        WRITE_TRACE_VARIABLE(out, 10, 3, f);
        WRITE_TRACE_VARIABLE(out, 11, 4, d);
        WRITE_TRACE_VARIABLE(out, 12, 5, u);
        WRITE_TRACE_BLOB(out, 12, 6, strlen(ptr), ptr);
        /* Fake buffer size */
        write_buffer_size(out, (void *)(size_t)(0xff + i), 10 * i);
        write_end_entry(out);
    }

    fclose(out);
}

void read_trace(const char *filename)
{
    trace_read_state *state = start_reading(filename);

    printf("names:\n");
    for (int i = 0; i < state->n_names; i++) {
        printf("  %s\n", state->names[i]);
    }
    printf("\ntypes:\n");
    for (int i = 0; i < state->n_types; i++) {
        type_description type = state->types[i];
        printf("  %s: %u, %u bytes\n",
               state->names[type.name_index], type.format, type.size);
    }

    int count = 0;
    while (1) {
        enum trace_entry_tag tag = read_tag(state);
        if (tag == END_OF_TRACE)
            break;

        switch (tag) {
        case END_ENTRY:
          count++;
          break;
        case STATEMENT_ID:
          printf("ID: %lu\n", read_id(state));
          break;
        case VARIABLE:
            {
                trace_var_info info = read_var_info(state);
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
                  printf("blob: '%.*s'\n", info.size, (const char *)info.value.ptr);
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
                printf("buffer size: %lx . %lu\n", info.address, info.size);
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

int main(int argc, char **argv)
{
    if (!strcmp(argv[1], "--write")) {
        write_test_trace(argv[2]);
    }
    else {
        read_trace(argv[1]);
    }

    return 0;
}
