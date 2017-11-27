#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/* Ensure the buffer will hold at least needed elements.
   Reallocates if necessary.
 */
void ensure_buffer_size(void **buffer, size_t element_size,
                        uint32_t *allocated, uint32_t needed);

/* Open a file and wait for it to have data available to read.

   For regular files this should return a file handle immediately. For FIFOs,
   it will return NULL if nothing is written to the pipe within the timeout
   interval.
 */
FILE *open_with_timeout(const char *filename, int timeout_seconds);

/* Allocate memory and fill it with a copy of buffer. */
void *malloc_copy(const void *buffer, size_t size);
