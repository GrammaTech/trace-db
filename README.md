# TRACE-DB

Writing, reading, storing, and searching of program traces (source and binary)

## Trace Format

A trace consists of a header followed by a series of trace points.

The header contains a name dictionary and a type dictionary. The string
dictionary consists of a character count (a 16-bit integer), followed by a
series of null-terminated strings. The type dictionary consists of a record
count (16 bits), followed by a series of `TypeDescription` objects.

A trace point is a sequence of trace entries, each representing a single item
such as a variable, buffer size, etc. Each entry begins with a one-byte tag,
indicating the kind of entry. A tag value of 0 indicates the end of the trace
point.

All entries within a trace point correspond to the same point in the program's
execution.

### Statement ID
The `STATEMENT_ID` tag is followed by a single 32-bit field which identifies
the program location of this trace point. The trace library makes no assumptions
about the contents of this field, so clients are free to use the bits as needed
(e.g. storing a file ID in the high bits and line number in the low bits).

### Buffer Sizes
The `BUFFER_SIZE` tag is followed by two 64-bit values representing an address,
and the size of the buffer allocated at that address.

### Auxiliary Data
The `AUXILIARY` tag is a followed by a single 64-bit value. This value
is not assigned any particular meaning by the trace library, and can
be used to associate additional information with each trace point.

### Variables
The `VARIABLE` tag marks a record of a program variable. It consists of a
name index, type index, size (optional), and value. The first two fields are
dictionary indices, indicating the name and type of the variable.

The value of a variable may be of arbitrary size. In most cases the size is
fixed for a given type, and is stored in the type record. But some types have
variable size, indicated by a 0 in the type record. In this case, the trace
contains a 16-bit field indicating the size of this particular value, followed
by that many bytes of data.

### Types
A type description consists of a name (represented by an index into the
dictionary), a format (e.g. signed integer, float, blob), and a size. For
more details, see the `TypeDescription` class in
[TypeDescription.hpp](TypeDescription.hpp)

## Internal representation

The trace database is a header-only library defined in the hpp files found
at the top-level of the project.  The existing sample.cpp file, useful for
debugging, remains at the top level and leverages this C++ library.
The hope is that this header-only library may be easily utilized by clients
looking to bring the trace database into other projects.

The main `Trace` class may be utilized to read and write and single
binary trace.  The `TraceDB` class is utilized to store a collection of these
traces.  Additionally classes are provided to represent the sub-elements
of the trace (e.g. TracePoints, TraceVarInfo, etc.).  For more information,
please consult the documentation in these files.  All objects are immutable.

The trace database leverages the boost flyweight library to reduce
memory usage and improve query performance.  The flyweight library implements
the flyweight design pattern in which objects are allocated within a pool
and shared by multiple references.  For more information, consult
"Design Patterns: Elements of Reusable Object-Oriented Software."
In this case, we mark trace point information (e.g. variables) as
flyweights to ensure sharing across multiple points when this
information does not change.  Additionally, when performing queries,
results are cached on the flyweight object to improve query performance
across duplicate points.

Beyond the header-only library, within the lisp/ directory exists an
additional C wrapper around the C++ library exposing a CFFI interface
to the trace database which is leveraged by additional common lisp code.
The trace database was originally designed for projects using common lisp,
necessitating this wrapper.

## Storing Traces

The `TraceDB` type and its associated methods can be used to store
and retrieve traces.  Traces may be written and read in a proprietary
binary format or using boost serialization.

## Queries

The `query` method searches a trace. A query consists of a set
of free variables, associated type constraints, and a predicate over
the free variables. At each trace point, the database finds all
combinations of valid assignments for the free variables, and collects
those assignments which satisfy the predicate.

For more information, please consult the documentation on the method
directly.
