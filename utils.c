#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "utils.h"

FILE *open_with_timeout(const char *filename, int timeout_seconds)
{
    /* Open in non-blocking mode. Returns immediately even if file is a FIFO
       with no writer yet. */
    int fd = open(filename, O_RDONLY | O_NONBLOCK);

    /* Now use select() to wait until there is data to read. For a regular
       file, this should return immediately. For a FIFO, it will block until
       some data is written. */
    fd_set set;
    FD_ZERO(&set);
    FD_SET(fd, &set);

    struct timeval timeout;
    timeout.tv_sec = timeout_seconds;
    timeout.tv_usec = 0;

    int result = select(fd + 1, &set, NULL, NULL, &timeout);

    if (result == 1) {
        /* Switch back to block mode */
        int flags = fcntl(fd, F_GETFL, 0);
        fcntl(fd, F_SETFL, flags & ~O_NONBLOCK);

        return fdopen(fd, "rb");
    }
    else {
        return NULL;
    }
}

void *malloc_copy(const void *buffer, size_t size)
{
    void *copy = malloc(size);
    memcpy(copy, buffer, size);
    return copy;
}
