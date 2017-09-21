% Trace DB Notes

Top level contents
- [Architecture](#architecture)
- [Trace Collection, Storage, and analysis Process](#trace-collection-storage-and-analysis-process)
- [Anticipated benefits](#anticipated-benefits)
- [Use Cases](#use-cases)
- [Requirements](#requirements)
- [Format Ideas](#format-ideas)
- [Miscellaneous](#miscellaneous)

Current Implementation Plan
1.  Design a simple binary format
2.  Implement a trivial trace collector which performs minimal
    pre-processing of traces before storing them
3.  Implement a simple query API
4.  Connect to SEL/Bug-injector

# Architecture

     +--------------+                  Search in      +----------+
     | Instrumented |                  query lang     |  Client  |
     |   Program    |              +------------------|          |
    1| ------------ |              v        5         |          |
     |  write to    |       +--------------+  results |          |
     |    pipe      |       |   TRACE-DB   |--------->|          |
     +--------------+       | -----------  |          +----------+
       2 | Binary           |              |
    pipe V Format           |  Hash        |
    +--------------+        |  Consing     |
    |  3 Trace     |        |              |
    |  Collector   | Writes |  Searchable  |
    | -----------  |------->|  Compression |
    | pre-process  |   4    |              |
    +--------------+        +--------------+

1.  Programs are instrumented (source or binary) to write trace data
    to a pipe.  Trace-db provides instrumentation as C which may be
    injected literally into program source, or compiled to a shared
    object and linked into a program.

2.  Program is run and trace data written to a pipe.

3.  Trace data is read and parsed by a trace collector.  Trace
    collector may also pre-process data before writing to the database
    (e.g., convert memory values to sizes of the pointed to regions).

4.  Trace data is written into the trace database.  This will perform
    hash consing and compression to reduce the resources required for
    trace storage and search.

5.  Client programs query the trace database to perform trace
    processing and analysis.

## API, points of contact/use for clients

-   Instrumentation to produce binary format.

-   Optional pre-processing.

-   Search of trace-db and results processing.

# Trace Collection, Storage, and analysis Process

Combine the insights of hash-consing and compression.

1.  [Instrumentation](#instrumentation).
2.  [Execution](#execution).

3.  **Preprocessing** for Compaction.
    -   *difference traces* (see [Blocking: Exploiting spatial locality
        for trace compaction](http://stonecat/repos/reading/agarwal1990blocking.html) and [Mache: No-loss trace compaction](http://stonecat/repos/reading/samples1989mache.html))
4.  **Hash-consing** (see [An interactive program verifier](http://stonecat/repos/reading/deutsch1973interactive.html)) we leverage
    uniform value-based pointers to save space in our representation of
    large sequences of variable/value pairs.  This turns the trace into
    a sequence of numbers.
5.  **Compression**.  From WPPs (see [Whole program paths](http://stonecat/repos/reading/larus1999whole.html)) we use
    something like their SEQUITUR(1) algorithm to compress the sequence
    of hash-cons pointers (numbers) representing the path.
    Alternately, maybe use any compression from which one can begin
    decompressing in the middle, and can recall by an AST ID.

## Instrumentation

Could be done with clang-instrument, but that won't be suitable for
all uses (e.g. when source is not available). The binary format should
be kept simple so we can easily inject code to write it whatever
context is necessary. We will also want to support other languages so
ideally it won't be too much work to reimplement the trace writing
functions.

## Execution

See the bit about enclosing wrappers below.

## Hash-Consing

This is maybe the most important piece.  The idea here is that every
encountered snapshot of the scope is represented by pointing to
previous scope snapshots.  The data structure is an alist.  Newer
snapshots can point their `cdr` to older snapshots.  Values from older
snapshots may be overridden in newer snapshots; e.g., pointing `foo`
to nil removes foo from previous snapshot while pointing `foo` to a
value overrides previous values of foo.

A hashing algorithm is used to calculate pointer values from the
contents (content addressable storage).  This ensure the same thing is
never stored more than once.

The result could be a single sequence of numbers (the pointer values
of the hashed conses).

## Compression/Storage

Use something like SEQUITUR(1) or LZ-END to compress then store as a
flat file, tree, dictionary, or a database?

Requirements

-   Perform analyses such as searching for encountered states satisfying
    certain criteria.
-   Quickly select subsets of traces based on the associated program
    location.

### Related, see the `pandalog`

-   <https://github.com/moyix/panda/blob/master/qemu/panda/pandalog.c>

# Anticipated benefits
## Compact storage

Both via hash-consing and then subsequent compression.

## Faster analysis

For two reasons.

1.  Memoized analysis/processing functions can cache the results of
    execution on hash-consed pointers.
2.  Running analysis on the compressed dictionary (instead of full
    traces) should allow for greatly reduced input sizes to analysis.
    (In effect we run analysis on the compressed data not on the full
    data.)

# Use Cases

Ideas from past and future GrammaTech projects.

## CodeSonarX

Tails of traces leading up to crash points.  Mainly interested in a
series of EAs or LoCs building up to the crash location.

Add-on to identify overflows, etc.
-   Use rewriting to make program vulnerable
-   Run, looking for overflows
-   Dump traces after each overflow

Traces are currently list of effective addresses. Long-term may have
variable or register values, allocation sizes.

Overflows are detected by scanning the traces linearly.

## CodeSonar regression test tracing

Run an entire test suite, collecting traces, then query for various
properties:
-   Which tests executed a given line of code?
-   Which values were assigned to a variable in a particular test or
    set of tests?

We might want to support strings, C++ standard containers, and other
non-primitive types.

Will access traces by loc and variable from the source code.  Might
need to care about scopes, but this can be reverse engineered from var
and loc.

## AER

Similar to CodeSonar X. Binary focused &#x2013; storing addresses,
registers and flags.

Will access traces to dump linear sequences by successive EA/LOC.

## Heterogeneous Computing

Use tracing to build call graphs from obfuscated code. Very simple,
just basic block IDs.

Will access traces to dump full linear sequence of basic block
transitions.

## Value-set analysis

Use an initial trace to initialize static VSA. Again, collecting
values of variables at each execution point.

## Bug Injector

Collect values of variables and sizes of memory regions.

Queries can be fairly complicated, looking for trace points which
satisfy arbitrary preconditions.

## Trace comparison

We have previously implemented tools to compare traces, see
[trace-cmp](https://git.grammatech.com/research/trace-cmp).
Supporting walking multiple traces in parallel and showing the
differences would be useful.

# Requirements

-   (maybe) Support various primitive types (signed and unsigned, different sizes)
-   Support binary blobs of arbitrary size (for tracing strings and data structures)
-   Query API
    -   Minimal for now, only meeting current needs
    -   Keep orthogonal to storage as much as possible

# Format Ideas

-   Dictionary of variable names
-   Dictionary of types
    -   Names
    -   Size
    -   Signedness (Eric thinks we skip this?)
    -   Any other necessary properties (Eric thinks we skip this?)
-   Type indices are variable size, defined in the trace header
-   Other details are generally similar to the current prototype

Maybe the keys into this database should be variable size determined
at instrumentation time based on the size of the dictionaries.

> Eric thinks we may not want to include signedness in the type
> database, rather this is a property *of* the type and is maybe
> something that the client should care about but we can let trace-db
> continue to treat everything as sequences of bytes.

# Miscellaneous
## "whole program paths"

See [Whole program paths](http://stonecat.grammatech.com/repos/reading/larus1999whole.html).

## diff storage

-   All functional data structures (maximize re-use)
-   Look into [hash-consing](https://en.wikipedia.org/wiki/Hash_consing)
    -   use the `:weak t` keyword argument to make-hash-table
-   For storage of the scopes/environments
    -   Use an alist, that way earlier values can shadow later values and
        we can keep tails of previous scopes unchanged
    -   Maybe something like a special out-of-scope value which we can
        insert to remove a value from a previous scope (if needed)
    -   Maybe something like a *previous environment* pointer which we can
        generically point back to?
-   Breaking on function boundaries

### Memoizing expensive trace calculation on ingestion

> We should keep in mind the desire to not repeat trace-based
> calculation (e.g., memory region maintenance) when we re-process a
> repeated trace region (assuming our diff representation).

### Incremental incorporation

[An interactive program verifier](file:///home/eschulte/reading/reading.md)

Introduces "hash-consing"

-   p.29 III-5 &#x2013; p.31 &#x2013;

    > The basic problem is to take a key (a variable or an expression), a
    > data base name (property name), and a context, and logically search
    > backwards toward the root looking for the first context in which
    > there is an entry (or cancellation, i.e., deliberate non-entry) for
    > that key in that data base. For example, in the clause data base the
    > keys are the clauses themselves and the associated value is a flag
    > indicating the presence and origin of the clause. PIVOT first takes
    > the key and looks it up in a hash table (the mechanism required to
    > make this work for expressions is described in Chapter V). The
    > corresponding "value" is a list of entries of the form (context
    > . value), sorted in such a way that if the context Cl is an ancestor
    > of C2, then the entry for Cl appears on the list after that for C2
    > if both are present. Cancellations are denoted by value=NIL. Now if
    > C is the context for which the value is desired, it is only
    > necessary to search down this list until one encounters an entry
    > where the context component is an ancestor of C (i.e., a "tail" of C
    > in the sense that (2 1) is a tail of (3 2 1)). Since a given
    > expression is likely only to have an entry in a few contexts, the
    > search is likely to be short: a run of PIVOT on a substantial
    > program gave an average of 3.98 entries examined per successful
    > lookup, and 2.44 per unsuccessful lookup.

## Concurrency

Can we handle multiple writers?
-   Trace collector can poll multiple pipes and keep traces separate
-   Can compaction handle multiple traces arriving in parallel?
    -   May cause sub-optimal results from hash consing
    -   If necessary could re-compact the database later

Probably no need for read/write concurrency at this time.

### See also

-   [Efficient program tracing](http://stonecat/repos/reading/larus1993efficient.html)
-   [Trace-driven memory simulation: A survey](http://stonecat/repos/reading/uhlig1997trace.html)
