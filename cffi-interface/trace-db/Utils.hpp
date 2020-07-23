//
// Utils.hpp -- Collection of utility functions and macros for
// the trace database.
//

#ifndef __UTILS_HPP
#define __UTILS_HPP

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <iostream>

#include <boost/iostreams/device/file_descriptor.hpp>
#include <boost/iostreams/stream.hpp>

#include "TraceError.hpp"

#define BINARY_READ(stream, ptr, size)                          \
    do {                                                        \
        if (!stream.good()) {                                   \
            if (stream.eof()) {                                 \
                throw TraceEOFError();                          \
            }                                                   \
            else {                                              \
                throw TraceError("Trace stream in bad state."); \
            }                                                   \
        }                                                       \
        stream.read( (char*) ptr, size );                       \
    } while (0)

#define BINARY_WRITE(stream, ptr, size)   \
    do {                                  \
        stream.write( (char*) ptr, size ); \
    } while (0)

/*
   Open a file and wait for it to have data available to read.

   Return a pointer to an istream on sucessful opening or throw
   a TraceError exception on failure.

   The client is responsible for deallocating the pointer.
 */
inline std::istream* openWithTimeout(const char *filename,
                                     uint64_t timeout_seconds) {
    namespace ios = boost::iostreams;

    /* Open in non-blocking mode. Returns immediately even if file is a FIFO
       with no writer yet. */
    int fd = open(filename, O_RDONLY | O_NONBLOCK);
    int result;

    /* Now use select() to wait until there is data to read. For a regular
       file, this should return immediately. For a FIFO, it will block until
       some data is written. */
    fd_set set;
    FD_ZERO(&set);
    FD_SET(fd, &set);

    if (timeout_seconds) {
        struct timeval timeout;
        timeout.tv_sec = timeout_seconds;
        timeout.tv_usec = 0;

        result = select(fd + 1, &set, NULL, NULL, &timeout);
    }
    else {
        result = select(fd + 1, &set, NULL, NULL, NULL);
    }

    if (result == 1) {
        /* Switch back to block mode */
        int flags = fcntl(fd, F_GETFL, 0);
        fcntl(fd, F_SETFL, flags & ~O_NONBLOCK);

        ios::file_descriptor_source fds(fileno(fdopen(fd, "rb")),
                                        ios::close_handle);
        ios::stream_buffer<ios::file_descriptor_source> *fpstream =
            new ios::stream_buffer<ios::file_descriptor_source>(fds);
        return new std::istream(fpstream);
    }
    else {
        /* Close file descriptor and throw an error */
        close(fd);
        throw TraceError(std::string(filename) + " could not be opened.");
    }
}

#endif // __UTILS_HPP
