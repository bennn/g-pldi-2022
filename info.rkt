#lang info
(define collection "g-pldi-2022")
(define deps '(
  "at-exp-lib"
  "base"
  "draw-lib"
  "gtp-plot"
  "gtp-util"
  "images-lib"
  "math-lib"
  "mf-apply"
  "pict-abbrevs"
  "pict-lib"
  "plot-lib"
  "ppict"
  "scribble-abbrevs"
))
(define build-deps '(
  "racket-doc"
  "rackunit-lib"
  "scribble-doc"
))
(define pkg-desc "PLDI 2022 sources and code")
(define version "0.1")
(define pkg-authors '(ben))
(define compile-omit-paths '("code" "src"))
(define test-omit-paths '("code" "src"))

