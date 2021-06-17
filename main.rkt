#lang at-exp racket/base

(provide
  appendixref
  if-techrpt
  ~a ~s
  exact
  $
  noindent
  ddeliverable
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
  parag
  rnd
  shorturl
  github-commit
  POPL-2017-BENCHMARK-NAMES
  PYBENCH
  SR
  snatural
  sforgetful
  sForgetful
  sNatural
  stransient
  sTransient
  sdeep
  sDeep
  sshallow
  sShallow
  suntyped
  sUntyped
  swrap
  sscan
  snoop
  figureref
  Figureref
  section-ref
  Section-ref
  sectionref
  Sectionref
  gtp-url
  make-lib
  render-lib
  user-inspiration
  nested-inset
  glob-first
  default-rkt-version
  transient-rkt-version
  github-issue
  github-pull
  (all-from-out scribble-abbrevs/scribble))

(require
  file/glob
  racket/match
  scribble/core
  scribble/base
  scriblib/figure
  (only-in scribble-abbrevs/scribble Integer->word integer->word format-url oxfordize)
  (only-in racket/string string-replace)
  (only-in racket/format ~a ~r ~s)
  (for-syntax racket/base))

;; -----------------------------------------------------------------------------

(define-syntax-rule (def2 k v)
  (begin (define k v) (define-for-syntax k v)))

(def2 TECHRPT #true)

(define-syntax if-techrpt
  (if TECHRPT
    (lambda (stx) (syntax-case stx ()
     [(_ x ...) #'(begin x ...)]))
    (lambda (stx) #'(void))))

(define appendixref
  (if TECHRPT
    (lambda (str) (secref str))
    (lambda (str) "supplementary material")))

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
(define figureref figure-ref)
(define Figureref Figure-ref)
(define section-ref secref)
(define Section-ref Secref)
(define sectionref section-ref)
(define Sectionref Section-ref)

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

(define (parag . x)
  (apply elem #:style "paragraph" x))

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

(define snatural "natural")
(define sNatural "Natural")
(define stransient "transient")
(define sTransient "Transient")
(define sforgetful "forgetful")
(define sForgetful "Forgetful")

(define sdeep "deep")
(define sDeep "Deep")
(define sshallow "shallow")
(define sShallow "Shallow")
(define suntyped "untyped")
(define sUntyped "Untyped")

(define swrap "wrap")
(define sscan "scan")
(define snoop "noop")

(define gtp-url
  @format-url{https://docs.racket-lang.org/gtp-benchmarks/index.html})

(struct lib [name url] #:prefab)

(define make-lib lib)

(define (render-lib lb)
  (define u (lib-url lb))
  (define name (tt (lib-name lb)))
  (if u
    (hyperlink u name)
    name))

(define ui-name car)
(define ui-title cadr)
(define ui-date caddr)
(define ui-url cadddr)

(define (user-inspiration tag data*)
  (define num-msgs (length data*))
  (nested-inset
    (list @elem{The examples in @section-ref[tag] are inspired by the following messages to the Racket-Users mailing list:}
          (apply itemlist
                 (for/list ((ui (in-list data*)))
                   (item
                     (elem (emph (ui-title ui))
                           ", sent by "
                           (ui-name ui)
                           " on "
                           (ui-date ui)
                           ". ")
                     (linebreak)
                     (format-url (ui-url ui))))))))

(define (nested-inset . content)
  (nested #:style 'inset content))

(define (glob-first str)
  (match (glob str)
    [(cons r '())
     r]
    ['()
     (raise-user-error 'glob-first "No results for glob '~a'" str)]
    [r*
     (printf "WARNING: ambiguous results for glob '~a'. Returning the first.~n" str)
     (car r*)]))

(define transient-rkt-version "7.8.0.5")
(define default-rkt-version transient-rkt-version)

(define (ddeliverable [D "D"])
  (define d-str
    (cond
     [(string? D)
      D]
     [(and (real? D) (positive? D))
      (number->string D)]
     [else
      (raise-argument-error 'ddeliverable "(or/c positive-real? string?)" D)]))
  (elem ($ d-str) "-deliverable"))

(define ((make-github-url kind) user repo name)
  (define url-str
    (format "https://github.com/~a/~a/~a/~a" user repo kind name))
  (define short-str
    (format "~a/~a #~a" user repo name))
  (hyperlink url-str (tt short-str)))

(define github-issue (make-github-url "issues"))
(define github-pull (make-github-url "pull"))

