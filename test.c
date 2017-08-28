#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "trace.h"

int main(int argc, char **argv)
{
    trace_read_state *state = start_reading(argv[1]);

    int end_count = 0;
    int count = 0;
    while (end_count < 2) {
        trace_entry entry = read_entry(state);
        switch (entry.kind) {
        case END_ENTRY:
          /* printf("end trace point\n\n"); */
          count++;
          end_count++;
          break;
        case STATEMENT:
          /* printf("statement %lu\n", entry.data); */
          break;
        case SCOPES:
          /* printf("scopes: %lu variables\n", entry.data); */
          for (unsigned int i = 0; i < entry.data; i++) {
              trace_var_info info = read_var_info(state);
              /* printf("%s %s %lu\n", */
              /*        string_lookup(state, info.name_index), */
              /*        string_lookup(state, info.type_index), */
              /*        info.value); */
              memcpy(malloc(sizeof(info)), &info, sizeof(info));
          }

          break;
        case SIZES:
          /* printf("sizes: %lu pointers\n", entry.data); */
          for (unsigned int i = 0; i < entry.data; i++) {
              trace_buffer_size info = read_buffer_size(state);
              memcpy(malloc(sizeof(info)), &info, sizeof(info));
              /* printf("%lux: %lu\n", info.address, info.size); */
          }
          break;
        }

        if (entry.kind != END_ENTRY)
            end_count = 0;
    }

    end_reading(state);

    printf("read %d trace points\n", count);
    return 0;
}
