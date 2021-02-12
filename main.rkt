#lang at-exp racket/base

(provide
  ~a
  exact
  $
  noindent
  Table-ref
  table-ref
  Figure-ref
  figure-ref
  figure*
  example-type-shape
  NSA-num-cores
  NSA-core-name
  NSA-core-speed
  NSA-RAM
  NSA-timeout-minutes
  benchmark-name*
  bm
  rnd
  shorturl
  github-commit
  POPL-2017-BENCHMARK-NAMES
  PYBENCH
  SR
  (all-from-out scribble-abbrevs/scribble))

(require
  scribble/core
  scribble/base
  scriblib/figure
  (only-in scribble-abbrevs/scribble Integer->word integer->word)
  (only-in racket/string string-replace)
  (only-in racket/format ~a ~r))

;; -----------------------------------------------------------------------------

(define SR "Shallow Racket")

(define benchmark-name* '(
  sieve
  forth
  fsm
  fsmoo
  mbta
  morsecode
  zombie
  dungeon
  jpeg
  zordoz
  lnm
  suffixtree
  kcfa
  snake
  take5
  acquire
  tetris
  synth
  gregor
  quadT
  quadU
))

(define (rnd n)
  (~r n #:precision '(= 2)))

(define (exact . items)
  (make-element (make-style "relax" '(exact-chars))
                items))

(define ($ . items)
  (apply exact (list "$" items "$")))

(define noindent
  (make-element (make-style "noindent" '()) ""))

;; TODO render the word "table" not "figure"
(define Table-ref Figure-ref)
(define table-ref figure-ref)

(define (example-type-shape #:type t-str #:shape s-str #:cost c-str)
  @exact{\begin{tabular}[t]{l@"@" {\hspace{1mm}}c@"@" {\hspace{1mm}}l}
    \(\stype\) & \(=\) & @tt[t-str]\!\!\!\!
  \\
    \(\ftagof{\stype}\) & \(=\) & @tt[s-str]\!\!\!\!
  \end{tabular}})

(define NSA-num-cores 4)
(define NSA-core-name "i7-4790")
(define NSA-core-speed "3.60GHz")
(define NSA-RAM "16GB")
(define NSA-timeout-minutes 10)

(define (github-commit user repo commit)
  (define url-str
    (format "https://github.com/~a/~a/commit/~a" user repo commit))
  (define short-commit
    (substring commit 0 7))
  (hyperlink url-str @tt[short-commit]))

(define (latex-escape str)
  (string-replace str "_" "\\_"))

(define (bm str)
  (exact (list "\\textsf{" (latex-escape (~a str)) "}")))

(define (shorturl a b)
  (hyperlink (string-append a b) (tt b)))

(define PYBENCH (hyperlink "https://pyperformance.readthedocs.io/" @tt{pyperformance}))

(define POPL-2017-BENCHMARK-NAMES '(
  call_method
  call_simple
  chaos
  fannkuch
  float
  go
  meteor
  nbody
  nqueens
  pidigits
  pystone
  spectralnorm
))


