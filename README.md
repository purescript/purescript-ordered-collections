# purescript-ordered-collections

[![Latest release](http://img.shields.io/github/release/purescript/purescript-ordered-collections.svg)](https://github.com/purescript/purescript-ordered-collections/releases)
[![Build status](https://github.com/purescript/purescript-ordered-collections/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/purescript-ordered-collections/actions?query=workflow%3ACI+branch%3Amaster)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-ordered-collections/badge)](https://pursuit.purescript.org/packages/purescript-ordered-collections)

Purely-functional map and set data structures.

_If you are looking to work with JavaScript Objects, see [`purescript-foreign-object`](https://github.com/purescript/purescript-foreign-object)._

## Installation

```
spago install ordered-collections
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-ordered-collections).

## Performance benchmarks

Run `npm run bench:join` to measure construction, ranges, bulk operations, and
subsequent access at 1,000 and 100,000 entries. Ordinary Map/Set construction and
updates are measured separately from singleton unions. Construction-plus-lookup
cases also measure the combined workload.
`npm run bench` retains the existing comparison with the historical implementation.

The suite validates contents and tree invariants during five untimed warmups and
fails on invalid results, then takes 20 timed samples with results retained.
Fixture construction and validation are outside timing except where construction
is the operation being measured.
Minibench requests GC before each case; GC occurring during samples is included.

To compare revisions, use the same Node version, compiler, dependency versions,
hardware, and benchmark source in both checkouts. Build first, then alternate at
least three fresh-process runs of each revision using:

```sh
node --expose-gc --input-type=module --eval "import('./output/Bench.Join/index.js').then(m => m.main())"
```

Record the commit IDs, `node --version`, `npx purs --version`, and complete output.
Compare repeated runs and variation, not a single mean. These deterministic
integer-key workloads are diagnostic, not a model of every application; include
application-specific keys and access patterns when making performance decisions.
For operations that take only a few microseconds, use timed batches to distinguish
small improvements from timer and harness overhead; the suite times individual calls.
Do not use elapsed-time thresholds as CI correctness tests on shared runners.
