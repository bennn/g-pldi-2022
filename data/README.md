data
===

Datasets and analysis script

- `7.8.0.5/`: Performance of Deep types alone on all benchmarks
- `transient/`: Performance of Shallow types alone on all benchmarks
- `nsa-2020-11-04/`: Performance of Deep + Shallow types on some benchmarks
- `nsa-2020-12-30/`: Performance of Deep + Shallow types on more benchmarks
- `analyze.rkt`: Script for formatting data.

To quickly view the Deep + Shallow data, run `racket analyze.rkt`


### How does the data relate to the figures in the paper?

Figure 15 uses the two Deep + Shallow datasets (`data/nsa-2020-11-04/*.rktd`
and `data/nsa-2020-12-30/*.out`). It groups all data lines that have the same
typed modules and counts the number of groups that run fastest with a mix
of both Deep and Shallow types.

Figure 16 combines `data/7.8.0.5/*.rktd` (Deep alone) with `data/transient/*.rktd`
(Shallow alone). It compares the worst-case average running times of Deep
and Shallow for each benchmark.

Figure 17 uses the same two datasets (`data/7.8.0.5/*.rktd` and
`data/transient/*.rktd`) but performs a different analysis, counting good paths
that go from fully-untyped to fully-typed by changing one module at a time.

The script `analyze.rkt` prepares data for these figures with three functions:

- Figure 15 ~ `get-3d-table`
- Figure 16 ~ `get-mixed-worst-table`
- Figure 17 ~ `get-mixed-path-table`


### What's in the data?

Every data file begins with a `#lang` line and contains several data lines.
Each data line encodes a benchmark configuration and lists several running times.

For example, here is the second data line from the file `transient/acquire-2020-08-24.rktd`:

```
("000000001" ("cpu time: 457 real time: 456 gc time: 101" "cpu time: 453 real time: 453 gc time: 93" "cpu time: 443 real time: 440 gc time: 91" "cpu time: 446 real time: 443 gc time: 88" "cpu time: 440 real time: 438 gc time: 90" "cpu time: 453 real time: 453 gc time: 103" "cpu time: 463 real time: 462 gc time: 100" "cpu time: 437 real time: 435 gc time: 87"))
```

- The string "000000001" encodes a configuration of the `acquire` benchmark.
  The benchmark has 9 modules, and so the string has 9 characters.
  This configuration is the one in which every module except the last module
  (alphabetically) is untyped.
  The one typed module, `tree.rkt`, used Shallow types / the Transient
  semantics.

- The first running time comes from the string `"cpu time: 457 real time: 456 gc time: 101"`.
  The running time is 457 milliseconds (cpu time).
  The other times are also in milliseconds.
  The full string comes from Racket`s `time` helper: <https://docs.racket-lang.org/reference/time.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._time%29%29>

In this example from the `transient/` directory, a `"0"` in a configuration encoding
means "untyped" and a `"1"` means "Shallow types".
The other directories use different configuration encodings:

- In `7.8.0.5/`, `"0"` means "untyped" and `"1"` means "Deep types"
- In `nsa-2020-11-04/` and `nsa-2020-12-30/`, `"0"` means "untyped"
  and `"1"` means "Deep types" and `"2"` means "Shallow types"

All directories list running times in the same way: as a string with
CPU times, real times, and GC times (each in milliseconds).
The CPU time is the running time.


