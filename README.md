g-pldi-2022
===

Sources & code for a conditionally-accepted PLDI 2022 paper: _Deep and Shallow
Types for Gradual Languages_

```
@inproceedings{g-pldi-2022,
  author={Greenman, Ben},
  title={Deep and Shallow Types for Gradual Languages},
  booktitle={{TBD}},
  pages={TBD},
  year={2022}
}
```

The code here is a version of Deep and Shallow types for Typed Racket that was used
for its evaluation in the paper (section 5). It is provided as-is for archival
and reproducibility purposes.

For the latest news on Deep and Shallow types for Typed Racket, visit the following link:

  <https://github.com/racket/typed-racket/pull/948>


### Getting Started

1. Look over the files provided with this artifact:
   - `g-pldi-2022/paper.scrbl` is the entry point for the paper's source.
   - `data/` contains performance data for section 5 of the paper. See
     `data/README.md` for more information.
   - `code/` contains benchmarks and info for git submodules.
2. Run `make init` to install Git submodules for the `code/` directory.
   Afterward, check that `code/` contains the following:
   - A version of Typed Racket with support for Deep and Shallow types.
     In particular, the file
     [`code/typed-racket/typed-racket-lib/typed-racket/defender/defender.rkt`](./code/typed-racket/typed-racket-lib/typed-racket/defender/defender.rkt)
     should exist and contain a function `defend-top` that rewrites typed code
     to defend itself with transient run-time checks.
   - Several libraries: `gtp-measure`, `gtp-plot`, `gtp-util`, and `require-typed-check`
3. Install a modern version of Racket from <https://download.racket-lang.org/>.
   I have tested with [Racket 8.3](https://download.racket-lang.org/racket-v8.3.html).
   Make sure that its `racket` and `raco` executables are on your PATH.
4. Run `make install` to add Deep and Shallow support to your Racket, to
   install the related libraries, and to compile the paper.


### Building the Code: Step-by-Step Instructions

1. Run `make pdf` to build `g-pldi-2022/paper.pdf`
2. Test Deep and Shallow types for Typed Racket:
   -  Go to the directory [`code/typed-racket/typed-racket-test/transient/pass`](./code/typed-racket/typed-racket-test/transient/pass).
      All tests here should compile and run `raco make -v FILE.rkt && racket FILE.rkt`
      (though some tests may fail).
   - Go to the directory [`code/typed-racket/typed-racket-test/transient/error`](./code/typed-racket/typed-racket-test/transient/error).
     All tests here should raise a run-time exception; `raco make -v FILE.rkt`
     should succeed and `racket FILE.rkt` should error.
   - Use these tests to try writing your own.
3. (Linux and macOS only) Run a small benchmark such as `morsecode`:
   - Open [`code/run-info.rkt`](code/run-info.rkt) in a text editor
   - Replace `PATH-TO-YOUR-RACKET-INSTALL` with an absolute path to the Racket you installed above
   - Replace `PATH-TO-REPO` with an absolute path to this repo
   - Uncomment the line for `morsecode` (or for another benchmark)
   - Change to the `code/` directory, run `sh run-benchmarks.sh`, watch and wait for it to finish
   - Inspect the new `*.out` files under `run-output/*/`
     (see `data/README.md` to learn about the output format)
   - To update figure 15 from the paper (mix of Deep and Shallow) with the new data:
     1. Replace the old `*.out` or `*.rktd` files for the benchmarks with your new `*.out` files.
        For `morsecode`: `mv run-output/**/*morsecode*.out ../data/nsa-2020-11-04/*morsecode*.rktd`.
        (The suffix does not matter, either `.out` or `.rktd` works.)
     2. Delete the old `*.3d` cache files for your benchmarks.
        (`rm ../data/nsa-2020-11-04/*morsecode*.3d`)
     3. Delete the cache file for figure 15: `rm src/with-cache/mixed-3d-best.rktd`
     4. Rebuild the paper: `make pdf`

