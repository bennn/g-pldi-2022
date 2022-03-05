code
===

Home for benchmarks and git submodules that this repo depends on.

The two main parts are the GTP benchmarks and Typed Racket:

- `benchmarks/`: Code for 21 benchmark programs, each with `typed`, `shallow`,
  and `untyped` variants. These are based on version 6.0 of the [GTP benchmark
  suite](https://docs.racket-lang.org/gtp-benchmarks/).
- `typed-racket/`: Typed Racket with support for Deep and Shallow types.


Other libraries include:

- `require-typed-check/`: For running the GTP benchmarks.
- `gtp-measure/`: For collecting performance data
- `gtp-plot/`: For plotting data
- `gtp-util/`: Other helper functions


Two helper scripts:

- `run-info.rkt` A checklist for running the benchmarks. To use, uncomment one
  or more targets and fill in the missing absolute paths.
- `run-benchmarks.sh` Main run script.

