#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/* Ensure the buffer will hold at least needed elements.
   Reallocates if necessary.
 */
#define ENSURE_BUFFER_SIZE(buffer, element_size, allocated, needed) \
    do {                                                            \
        if ((allocated) < (needed)) {                               \
            (allocated) = ((allocated) == 0) ? 1024 : 2 * (allocated);  \
            (buffer) = realloc((buffer), (allocated) * (element_size)); \
        }                                                               \
} while (0)

/* Open a file and wait for it to have data available to read.

   For regular files this should return a file handle immediately. For FIFOs,
   it will return NULL if nothing is written to the pipe within the timeout
   interval.
 */
FILE *open_with_timeout(const char *filename, int timeout_seconds);

/* Allocate memory and fill it with a copy of buffer. */
void *malloc_copy(const void *buffer, size_t size);
