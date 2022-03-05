#lang gtp-measure/manifest

;; To run a benchmark:
;; - Fill in the missing absolute paths below
;; - Uncomment any benchmarks that you wish to run
;; - Run the companion script: run-benchmarks.sh

#:config #hash(
  (bin . "/PATH-TO-YOUR-RACKET-INSTALL/bin/")
  (cutoff . 20)
  (entry-point . "main.rkt")
  (iterations . 8)
  (jit-warmup . 1)
  (time-limit . #f) ; seconds
  (num-samples . 5)
  (sample-factor . 10))


;; Example benchmark, recommended for testing:
;("/PATH-TO-REPO/code/benchmarks/morsecode"  . deep-shallow-untyped)

;; Smaller benchmarks. Each should finish within a few hours:
;("/PATH-TO-REPO/code/benchmarks/forth"      . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/fsm"        . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/fsmoo"      . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/mbta"       . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/sieve"      . deep-shallow-untyped)

;; Mid-size benchmarks. Each should finish in a few days:
;("/PATH-TO-REPO/code/benchmarks/acquire"    . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/dungeon"    . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/jpeg"       . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/kcfa"       . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/lnm"        . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/snake"      . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/suffixtree" . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/take5"      . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/tetris"     . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/zombie"     . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/zordoz"     . deep-shallow-untyped)

;; Large benchmarks. May take weeks or months:
;("/PATH-TO-REPO/code/benchmarks/gregor"     . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/quadT"      . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/quadU"      . deep-shallow-untyped)
;("/PATH-TO-REPO/code/benchmarks/synth"      . deep-shallow-untyped)

