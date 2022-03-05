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
   - `paper.scrbl` is the entry point for the paper's source. Most other files
     in the toplevel directory of this repo implement the paper.
   - `data/` contains performance data for section 5 of the paper. See
     `data/README.md` for more information.
   - `code/` contains benchmarks and info for git submodules.
   - `src/` contains other code and old code.
2. Run `make init` to install Git submodules for the `code/` directory.
   Afterward, check that `code/` contains the following:
   - A version of Typed Racket with support for Deep and Shallow types.
     In particular, the file
     [`src/typed-racket/typed-racket-lib/typed-racket/defender/defender.rkt`](./src/typed-racket/typed-racket-lib/typed-racket/defender/defender.rkt)
     should exist and contain a function `defend-top` that rewrites typed code
     to defend itself with transient run-time checks.
   - Several libraries: `gtp-measure`, `gtp-plot`, `gtp-util`, and `require-typed-check`
3. Install a modern version of Racket from <https://download.racket-lang.org/>.
   Make sure that its `racket` and `raco` executables are on your PATH.
4. Run `make install` to add Deep and Shallow support to your Racket, to
   install the related libraries, and to compile the paper.


### Building the Code: Step-by-Step Instructions

1. Run `make pdf` to build `paper.pdf`
2. Test Deep and Shallow types for Typed Racket:
   -  Go to the directory [`code/typed-racket/typed-racket-test/transient/pass`](./code/typed-racket/typed-racket-test/transient/pass).
      All tests here should compile and run: `raco make -v FILE.rkt && racket FILE.rkt`
   - Go to the directory [`code/typed-racket/typed-racket-test/transient/error`](./code/typed-racket/typed-racket-test/transient/error).
     All tests here should raise a run-time exception; `raco make -v FILE.rkt`
     should succeed and `racket FILE.rkt` should error.
   - Use these tests to try writing your own.
4. Run a small benchmark such as `morsecode`:
   - Open [`code/run-info.rkt`](code/run-info.rkt) in a text editor
   - Replace `PATH-TO-YOUR-RACKET-INSTALL` with an absolute path to the Racket you installed above
   - Replace `PATH-TO-REPO` with an absolute path to this repo
   - Uncomment the line for `morsecode` (or for another benchmark)
   - Change to the `code/` directory, run `sh run-benchmarks.sh`, watch and wait for it to finish
   - Inspect the new `*.out` files under `run-output/*/`
   - Refer to the `gtp-measure` docs for help (`raco docs gtp-measure`)

