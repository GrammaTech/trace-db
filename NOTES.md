# Trace Collection, Storage, and analysis Process

Combine the insights of hash-consing and compression.

1.  **Instrumentation**.
2.  **Execution**.
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

Could be done with clang-instrument (or in other contexts with binary
rewriting).

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

# Enclosing wrappers to automate trace collection

-   [X] trace path into instrumented program on ENV
-   [X] `traceable` software-object class
-   [X] Serialization and parsing using either
    -   cl-conspack, or
    -   [series](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node347.html)
    -   [X] simple `read`

# TODO Read "whole program paths"

See [Whole program paths](http://stonecat.grammatech.com/repos/reading/larus1999whole.html).

# TODO diff storage

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

## Memoizing expensive trace calculation on ingestion

> We should keep in mind the desire to not repeat trace-based
> calculation (e.g., memory region maintenance) when we re-process a
> repeated trace region (assuming our diff representation).

## Incremental incorporation

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

## See also

-   [Efficient program tracing](http://stonecat/repos/reading/larus1993efficient.html)
-   [Trace-driven memory simulation: A survey](http://stonecat/repos/reading/uhlig1997trace.html)
