//
// TraceError.hpp - Definition of several errors related
// to trace reading; (1) a general TraceError and (2)
// an unexpected end-of-file error.
//

#ifndef __TRACE_ERROR_HPP
#define __TRACE_ERROR_HPP

#include <stdexcept>

class TraceError : public std::runtime_error
{
public:
    TraceError(const std::string & what) :
        std::runtime_error(what)
    {}
};

class TraceEOFError : public std::runtime_error
{
public:
    TraceEOFError() :
        std::runtime_error("Unexcepted trace end-of-file.")
    {}
};

#endif // __TRACE_ERROR_HPP
