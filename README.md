# TRACE-DB

Writing, reading, storing, and searching of program traces (source and binary)

See [NOTES](NOTES.md) for plans and design discussions.

## Trace Format
A trace consists of a header followed by a series of trace points.

The header contains a name dictionary and a type dictionary. The string
dictionary consists of a character count (a 16-bit integer), followed by a
series of null-terminated strings. The type dictionary consists of a record
count (16 bits), followed by a series of `type_description` structs.

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
more details, see the `type_description` struct in [types.h](types.h)

## Storing Traces
The `trace_db` type and its associated functions can be used to store
and retrieve traces. Traces are stored in memory, in a format that is
convenient for queries. Clients using the C API may access stored
traces directly, but the representation of traces may change over
time. The recommended method for accessing traces is through queries.

## Queries

The `query_trace` function searches a trace. A query consists of a set
of free variables, associated type constraints, and a predicate over
the free variables. At each trace point, the database finds all
combinations of valid assignments for the free variables, and collects
those assignments which satisfy the predicate.

The result is returned as a set of trace points, which each point
contains only a statement ID and the information for the bound
variables.

Queries can also be restricted by statement ID.

Finally, query trace has a `pick` mode which randomly selects one
trace point to query.
