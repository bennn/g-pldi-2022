#lang at-exp slideshow

;; TODO schedule a January (early???!) seminar with Christos
;; hi-ya ... jan 17 is earliest no way can do sooner
;; lol jan 29 first draft finally done
;; lol feb  3 still working on draft 2 / cleanup

;; 2022-02-03 SK hints
;; - no more than 30 minutes on gradual typing
;; - you MUST show evidence of branching out beyond the theme,
;;    "ben is a pl person we other profs can actually talk to" etc.
;; - 
;;
;; - add slides on LTL + language levels (NO gradual, NO typing)
;;   - spent last year enhancing repetoire
;;   - not yet a trained HF person but starting to make that transformation
;;     and have mentors + collaborators to assist
;;   - LTL ....
;;   - Alloy ....
;;   - small ed connection,
;;   - seriously looking at what people do
;;
;; consider a joke at the start, or ANYTHING to signal more than GT coming
;;  going to talk about phd work, taking industry by storm
;;  (facebook invented GT, video no longer available https://youtu.be/2mnYf7L7Amw?t=2525)
;;  for those who don't care about PL or types feel free to read email now (no not really)
;;  I'll talk about other things in 30 minutes


;; title: Mixing Typed and Untyped Code: A Tale of Proofs, Performance, and People
;; .... thanks SK
;; .... remember, this talk is going to Cornell, BU, Penn ... impress!

;; 2022-01-10 concrete slides
;; X samet babel
;;   1960, 1969
;;   rings true today (more on that soon) ... all these pls sometimes looks like a zoo
;;   but not necessarily bad if there's an organization
;;    pioneering techniques
;;    (incremental progress, need a lang to build a lang (but bootstrap), knowledge ~ language)
;;    really this picture is / would be terrific ... IF ONLY
;;   \exists big questions to solve ... tower is evidence of difficult and exciting problems
;;   really the challenge to keep building a tower as we tackle big questions
;;    very easy to go any which way
;; X Q. should your PL be typed
;;   one of the big endearing questions
;; X ask some wise people its obvious, Lamport Paulson etc.
;;   yet T hasn't taken world by storm
;; X look at github, people use untyped
;;   clearly SOMETHING there
;;   graduates / interns likely to find a job there
;; X both have benefits ... even Guido changed his mind ... we have impasse
;; X which was clear to researchers going way back (CL)
;;   [[ not a new guido insight! ]]
;;   and researchers in recent memory (2006 x4)
;;   and today the demand is so great that companies x10 are investing in in-house solutions
;;   lively but disorganized! ain't no tower
;; X my research is in this space
;;   yes new lang, but also ...
;;   central theme = methods
;;   next: perf. + guar. and then lang. design
;; X preview of final picture?
;; - EXAMPLE of the variety
;;   - ... skimp on guarantees, example
;;   - ... or skimp on expressiveness, example
;;   - ... really 3 parts to design question

;; design-space first
;; X example of disagreement -> what is disagreement -> characterizing theorems
;; - lots of variety
;;   ... model these with formal semantics? no way too much detail yet
;; - what old theorems (N T E)
;; - N T concerning
;; - click plot example? fold file example?
;;   - both sound, yet something majorly wrong with #2
;; - need a new property, 
;;   => table transformations:
;;      + (where is the technical depth?)
;;      + 

;;   X many designs, fragmented space, weak guarantees
;;     X why? perf
;;   X TR one of the first and best, pioneer
;;     X mixed-typed pitfalls! (sidebar) 25x, 12sec - 1ms
;;   X gee aware of costs, high costs, yet --- only measure top and bottom
;;   X what about space in between? well its BIG (lattice)
;;   X only one thing to do, measure!
;;   X thats what we did, lots of benchmark programs
;;   X learned quite a lot, developed method
;;     X bad points dont matter, too slow
;;     X starting from bottom, so look at the overhead
;;     X compress to %, D-deliverable
;;   X exhaustive method = D-deliv, nice plots
;;   X still clearly sscale problem
;;     - observation: bernoulli rv
;;     - sampling ought to work
;;     - tested with N ... configs
;;   X nice method! (steps 1 2 3 from diss?)
;;   X summary: performance work
;;     + problem: pitfalls exist, need to find them
;;     + tech challenge: need unbiased and scalable measure (and visualization!)
;;     + contrib: 1 2 3 lattice, D-deliv, %D, o-plot

;; (transition = sad that clearly no best answer, but again have a method and leads )
;; - contrib: 3way language "first of many" [[ may gloss over, it's not exactly a 3p ... but it does tie together! ]]
;;   + problem: impasse, guarantees and performance
;;     ... shown you 2 solutions but we still have a problem! (big picture, transition)
;;   + contribution: model, 3way works
;;   + contribution: implementation for TR

;; X 3P what about people#3
;;   X qualitative
;;   X rational programmer
;; X qualititive
;;   - ask people what think
;;     - challenge = who and what to ask, because relatively new language
;;     - dls 18 survey : comparing theory, like expect; NO PERF
;;     - tr survey : who are the users, where do they get stuck?
;;     - instagram gt ... generally : potentially many users, do design choices pay off?
;;       (we DO have some adoption data for IG, leverage that!)
;;     - (collaborators ... pictures
;;   - [-] need collaborators? or save until end is ok? NO NO waste of time
;;   - [-] proofs collaborators pict?
;;   - no not worth the time, they'ne not the ones applying for a job
;; - [x] don't use exactly the defense examples, probably
;;       at least change the colors
;;       plot-line-color jack-o-lantern
;; - [ ] more rmlo
;; - [ ] hmph, ALL the quick topics need backup slides
;; - [ ] thetalk.txt

;; src
;; file:///Users/ben/code/php-blg59/resources/pdf/hiring-2021.pdf
;; file:///Users/ben/code/php-blg59/resources/pdf/g-dissertation-2020-slides.pdf

;; misc thoughts
;; - what to do?
;;   + stay with current, try to debug
;;   + switch to tomorrow, move everything, deal with employee attrition (people love tools)
;;     replace utilities and libraries,
;;     huge investment --- "stay" is more practical
;;   + of course next lang is not FINAL lang gotta move again
;;     - Samet tower of babel not going away
;; - hence my research, area of GT, bridging gap from U to T specifically
;;   + think {Python JS} >> {Java Haskell} ... TypeScript is right on
;;   + long-term vision is systematic bridging
;;   ... PL is infrastructure, build programs on top
;;   ... typed vs untyped usually baked in
;;   ... need a better infrastructure to put T/U choice in hands of developers => bridge
;; - head off TypeScript with a slide?
;; - GT not so easy
;;   + mess of problems (flashy slide?)
;;   + boils down to PPP
;;     - full enforce = slow
;;     - no enforce = unsound
;;     - no interactions = inexpressive
;;   ... many possible / viable bridges, have competing strengths


;; =============================================================================

(require
  "slide-style.rkt"
  pict
  pict-abbrevs pict-abbrevs/slideshow
  ppict/2
  racket/draw
  racket/list
  racket/string
  racket/format
  racket/runtime-path
  scribble-abbrevs/pict)

(define pplay-steps (make-parameter 10))

(define ((slide-assembler/background base-assembler make-rect) slide-title slide-vspace slide-pict)
  (define foreground-pict (base-assembler slide-title slide-vspace slide-pict))
  (define background-pict
    (let ((+margin (* 2 margin))
          (-margin (- margin)))
      (inset (make-rect (+ +margin client-w) (+ +margin client-h)) -margin)))
  (cc-superimpose background-pict foreground-pict))

(define bg-orig (current-slide-assembler))
(define bg-cs.brown.edu (slide-assembler/background bg-orig make-cs.brown.edu))
(define title-cs.brown.edu (slide-assembler/background bg-orig make-titlebg))

;; =============================================================================

(define (do-show)
  (set-page-numbers-visible! #true)
  (set-spotlight-style! #:size 60 #:color (color%-update-alpha highlight-brush-color 0.6))
  [current-page-number-font page-font]
  [current-page-number-color white]
  ;; --
  (parameterize ((current-slide-assembler title-cs.brown.edu))
    (sec:title)
    (void))
  (parameterize ((current-slide-assembler bg-cs.brown.edu)
                 (pplay-steps #;3 ;; pdf
                              7 ;; show
                              ))
    #;(sec:babel)
    (sec:typed-untyped)
    (sec:case-closed)
    #;(sec:my-research)
    #;(sec:design-space)
    (sec:perf)
    #;(sec:expr)
    (sec:both)
    (sec:end)
    (void))

  (parameterize ((current-slide-assembler bg-cs.brown.edu))
    (pslide)
    (sec:alloy2)
    (sec:qa)
    (pslide)
    (void))

  (void))

;; -----------------------------------------------------------------------------

(define (pplay #:steps [N (pplay-steps)]
               #:delay [secs 0.05]
               #:skip-first? [skip-first? #f]
               mid)
  (unless skip-first?
    (pslide #:set (mid ppict-do-state 0)))
  (if condense?
      (skip-slides N)
      (for ([n (in-list
                (let ([cnt N])
                  (let loop ([n cnt])
                    (if (zero? n)
                        null
                        (cons (/ (- cnt -1 n) 1.0 cnt)
                              (loop (sub1 n)))))))])
        (pslide #:timeout secs
                #:set (mid ppict-do-state n)))))

(define the-title-str "Mixing Typed and Untyped Code: A Tale of Proofs, Performance, and People")

(define (sec:title)
  (pslide
    #:next
    #:go (coord 1/2 26/100 'ct)
    (let* ([title-pict
             (browncs-box
               #:y-margin small-y-sep
               (let* ((str* (string-split the-title-str ": "))
                      (top (car str*))
                      (bot (cadr str*)))
                 (vl-append
                   tiny-y-sep
                   (title-block top)
                   (ht-append (xblank small-x-sep) (subtitlerm bot)))))]
           [tu-pict
             (vc-append
               pico-y-sep
               (boundary-node '(U D U D D))
               (boundary-node '(U U D D))
               #;(boundary-node '(D D U U D)))]
           [ben-pict (vr-append -4 @subtitlerm{Ben Greenman} @subtitlerm{2022-04-15})]
           [brown-pict (scale-to-width% (bitmap "img/browncs-logo.png") 20/100)]
           [author-pict (browncs-box (hc-append small-x-sep ben-pict brown-pict))])
      (vr-append
        tiny-y-sep
        (vc-append tiny-y-sep title-pict tu-pict)
        author-pict))
  )
  (void))

(define (sec:babel)
  (pslide
    #:go center-coord
    @bodyrmlo{?? heads up about non-GT topics ahead}
    )
  (pslide
    #:go center-coord (tag-pict (browncs-box (babel-acm)) 'babel)
    #:go (at-find-pict 'babel rc-find 'ct #:abs-x (- small-x-sep) #;#;#:abs-y tiny-y-sep)
    (browncs-box (vc-append 0 @bodyrmhi{Jean Sammet} @bodyrmhi{1960}))
    )
  (pslide
    #:go center-coord (browncs-box (babel-full))
    #:alt (
    #:go (coord 59/100 48/100 'cc) (browncs-box babel-zoom)
    #:next
    #:go (coord slide-left 1/2 'lb)
    (browncs-box
      (vl-append
        @headrm{Grand PL Vision:}
        (left-line-append2
          @bodyrm{ A tower of languages, each one}
          (word-append @bodyembf{ building upon} @bodyrm{ previous steps})
          (word-append @bodyrm{ to approach hard problems})
          (yblank tiny-y-sep))))
    )
    ;; tower all well and good,
    ;; but you may wonder why so many
    ;; it's because theses are HARD problems dealing with
    ;; my work focuses on one ... hard, longstanding ... 
    #:go center-coord
    (question-pict @bodyrm{Should your language be typed?})
    )
  (void))

(define (sec:typed-untyped)
  (pslide
    #:go (coord 1/2 1/2 'cc)
    (ppict-do
      (browncs-frame
        (freeze
          (scale-to-height (babel-acm-pict) (h%->pixels 7/10))))
      #:go (coord 80/100 10/100 'cc)
      (browncs-box
        @headrm{Typed or Untyped?}))
  )
  (pslide
    #:go heading-coord-m
    (typed-or-untyped-text)
    #:next
    #:go center-coord
    (tag-pict (vrule (h%->pixels 5/10)) 'thebar)
    #:go (coord slide-text-left slide-text-top 'lt)
    (vl-append
      small-y-sep
      (blank)
      (ht-append
        tiny-x-sep
        (java-pict)
        (vl-append 2
          (word-append
            @bodyrm{Java } @bodyrmlo{is} @bodyrm{ typed})
          @bodyrmlo{  (statically typed)}))
      (tag-pict (java-codeblock) 'javacode)
    )
    #:go (coord (+ 1/2 slide-text-left) slide-text-top 'lt)
    (vl-append
      small-y-sep
      (blank)
      (ht-append
        tiny-x-sep
        (js-pict)
        (vl-append 2
          (word-append
            @bodyrm{JavaScript } @bodyrmlo{is} @bodyrm{ untyped})
          @bodyrmlo{  (dynamically typed)}))
      (tag-pict (js-codeblock) 'jscode)
    )
    #:next
    #:go (at-find-pict 'javacode lb-find 'lt #:abs-y tiny-y-sep #:abs-x 0)
    (vl-append
      2
      (word-append
        @bodyrmhi{With} @bodyrmlo{ types, languages can:})
      (plus-bullet
        @bodyrmlo{Prevent classes of bugs})
      (plus-bullet
        @bodyrmlo{Support tools}))
    #:next
    #:go (at-find-pict 'jscode lb-find 'lt #:abs-y tiny-y-sep #:abs-x 0)
    (vl-append
      2
      (word-append
        @bodyrmhi{Without} @bodyrmlo{ types, programmers can:})
      (plus-bullet
        @bodyrmlo{Focus on the code})
      (plus-bullet
        @bodyrmlo{Build flexible systems}))
    #:next
    #:go (at-find-pict 'thebar cb-find 'ct #:abs-y (- tiny-y-sep))
    (browncs-box
      (vc-append 2
        (word-append
          @bodyrmlo{Either way, } @bodyrmhi{long-term implications} @bodyrmlo{ for })
        (word-append
          @bodyrmlo{development and maintenance})))
  )
  (pslide
    #:go heading-coord-mid
    (typed-or-untyped-pict #:x '())
    (yblank tiny-y-sep)
    @bodyrmlo{Strong support for both sides}
    #:next
    #:alt (
    #:go (coord 15/100 36/100 'lt)
    (hc-append
      tiny-x-sep
      (LR-stack
        (typed-person-pict "img/lamport.png")
        (typed-person-pict "img/paulson.png"))
      (right-line-append
        (bodyrm "\"The advantages of typed PLs are obvious\"")
        @bodyrmlo{Lamport & Paulson, TOPLAS 1999  }
        (yblank med-y-sep)
        ))
    )
    #:next
    #:go (coord 85/100 32/100 'rt)
    (ht-append
      med-x-sep
      (vc-append
        tiny-y-sep
        (scale-to-square
          (bitmap "img/githut-pr.png")
          (h%->pixels 75/100))
        @tcoderm{https://madnight.github.io/githut/#/pull_requests/2021/4})
      (vc-append
        small-y-sep
        (untyped-codeblock* (list
        (m-line-append
          #:sep 2
          @bodyrm{Untyped PLs dominate}
          @bodyrm{on GitHub})))
        (lang-rankings
          "Ruby" (ruby-pict)
          "Python" (python-pict)
          "JavaScript" (js-pict)
          "PHP" (php-pict)
          "Java" (java-pict))))
    ;; long story short, want both
  )
  (let* ((mid (yblank big-y-sep))
         (bot (bghost (typed-and-untyped-pict #:x '(gt lbl)))))
  (pslide
    #:go center-coord
    #:alt ( (typed-or-untyped-pict #:x '(lbl)) mid bot )
    (typed-or-untyped-pict #:x '(lbl)) mid (typed-and-untyped-pict #:x '(gt lbl))
  ))
  (pslide
    #:go heading-coord-mid (typed-and-untyped-pict #:x '(allgt))
    #:go slide-text-coord-l
    #:go hi-text-coord-l
    (yblank pico-y-sep)
    (word-append
      @bodyrmlo{Key Motivation: } @bodyrmhi{improve stable code} @bodyrmlo{ with types})
    #:next
    (yblank tiny-y-sep)
    #:alt (
      (xindent (tag-pict (jpeg-untyped) 'ju))
      #:next
      #:go (at-find-pict 'ju lt-find 'lt #:abs-x tiny-x-sep #:abs-y (+ pico-y-sep small-y-sep))
      (tag-pict (jpeg-typed) 'jt)
      #:go (at-find-pict 'jt rt-find 'rt #:abs-x (- pico-x-sep) #:abs-y (+ pico-y-sep small-y-sep))
      (browncs-box
        (vc-append 2
          @bodyrmlo{Document the parameters,}
          @bodyrmlo{benefit from type checks}))
    )
    #:next
    #:go hi-text-coord-m
    (yblank pico-y-sep)
    (bghost @bodyrmlo{K})
    (yblank small-y-sep)
    #:alt (
      #:alt ( (big-program-example 0) )
      (ppict-do
        (big-program-example 1)
        #:go (coord 0 1 'lb)
        (browncs-box
          (vc-append
            2
            (word-append @bodyrmlo{Add types to } @bodyrmhi{one} @bodyrmlo{ component,})
            (word-append @bodyrmlo{leave the others } @bodyrmhi{unchanged}))))
    )
    #:next
    (tag-pict (huge-program-example) 'thecode)
    #:next
    #:go (at-find-pict 'thecode) 
    (guido-pict)
  )
  (pslide
    #:go heading-coord-mid (typed-and-untyped-pict #:x '(allgt))
    #:next (yblank tiny-y-sep) @bodyrm{Active space!}
    #:next
    #:go (coord 12/100 42/100 'ct) (cl-pict)
    #:go (coord 24/100 28/100 'ct) (strongtalk-pict)
    #:next
    #:go (coord 50/100 (+ 11/100 slide-bottom) 'cb)
    (label-below
      (scale (mt-originals) 70/100)
      @bodyrmlo{Gradual Typing}
      @bodyrmlo{2006})
    #:next
    #:go (coord 95/100 1/2 'rc)
    (label-below
      (scale (academic-lang-sidebar) 90/100)
      @bodyrmlo{+ Many Research PLs})
  )
  (pslide
    #:go heading-coord-mid (typed-and-untyped-pict #:x '(allgt))
    (yblank tiny-y-sep) @bodyrm{Active space!}
    (yblank pico-y-sep) @bodyrm{Major companies involved}
    #:next
    #:alt (
      (yblank small-y-sep)
      (industry-support-pict)
    )
    (yblank pico-y-sep) @bodyrm{Growing community interest}
    #:next
    (yblank tiny-y-sep)
    #:alt ( (dt-pict 0) )
    (dt-pict 1)
  )
  (pslide
    #:go (coord 1/2 92/100 'cb)
    (hrule (w%->pixels 7/10))
    (yblank tiny-y-sep)
    (all-lang-stripe)
    #:alt (
      #:go heading-coord-mid (typed-and-untyped-pict #:x '(allgt))
      (yblank (+ med-y-sep tiny-y-sep))
      (word-append
        @bodyrmlo{So what's the } @bodyembf{problem} @bodyrmlo{?})
      #:next
      (yblank tiny-y-sep)
      (word-append
        @bodyrmlo{Lots of Languages, but also } @bodyembf{Lots of Variety})
    )
    #:go heading-coord-l @headrm{Example 1}
    #:go (coord 1/2 slide-text-top 'ct)
    #:alt (
      (basic-example 'T #f)
      #:next
      (yblank small-y-sep)
      (n-q-pict)
    )
    (basic-example 'T 'U)
    (yblank small-y-sep)
    (n-q-pict)
  )
  (pplay
    #:skip-first? #t
    (lambda (pp step-n)
      (ppict-do
        pp
        #:go heading-coord-l @headrm{Example 1}
        #:go (coord 1/2 92/100 'cb)
        (hrule (w%->pixels 7/10))
        (yblank tiny-y-sep)
        (cc-superimpose
          (cellophane (all-lang-stripe) (- 1 step-n))
          (cellophane (nnum-lang-stripe) step-n))
        #:go (coord 1/2 slide-text-top 'ct)
        (basic-example 'T 'U)
        (yblank small-y-sep)
        (n-q-pict)
        (yblank tiny-y-sep)
        (word-append
          @bodyrmlo{Some say } @bodyembf{yes} @bodyrmlo{, others say } @bodyembf{no})
        )))
  (pslide
    #:go heading-coord-l @headrm{Example 1}
    #:go (coord 1/2 92/100 'cb)
    (hrule (w%->pixels 7/10))
    (yblank tiny-y-sep)
    (nnum-lang-stripe 1)
    #:go (coord 1/2 slide-text-top 'ct)
    (basic-example 'T 'U)
    (yblank small-y-sep)
    (n-q-pict)
    (yblank tiny-y-sep)
    (word-append @bodyrmlo{Some say } @bodyembf{yes} @bodyrmlo{, others say } @bodyembf{no})
  )
  (pslide
    #:go heading-coord-r @headrm{Example 2}
    #:go (coord 1/2 92/100 'cb)
    (hrule (w%->pixels 7/10))
    (yblank tiny-y-sep)
    (all-lang-stripe)
    #:next
    #:go (coord 1/2 slide-text-top 'ct)
    (bad-array-example)
    (yblank small-y-sep)
    #:next
    (arr-q-pict)
  )
  (pplay
    #:skip-first? #true
    (lambda (pp n)
      (ppict-do
        pp
        #:go heading-coord-r @headrm{Example 2}
        #:go (coord 1/2 92/100 'cb)
        (hrule (w%->pixels 7/10))
        (yblank tiny-y-sep)
        (cc-superimpose
          (cellophane (all-lang-stripe) (- 1 n))
          (cellophane (arrn-lang-stripe) n))
        #:go (coord 1/2 slide-text-top 'ct)
        (bad-array-example)
        (yblank small-y-sep)
        (arr-q-pict)
        (yblank tiny-y-sep)
        (word-append
          @bodyrmlo{Three common answers: } @bodyembf{yes} @bodyrmlo{, } @bodyembf{no} @bodyrmlo{, and } @bodyembf{sort of})
    )))
  (pslide
    #:go heading-coord-r @headrm{Example 2}
    #:go (coord 1/2 92/100 'cb)
    (hrule (w%->pixels 7/10))
    (yblank tiny-y-sep)
    (arrn-lang-stripe 1)
    #:go (coord 1/2 slide-text-top 'ct)
    (bad-array-example)
    (yblank small-y-sep)
    (arr-q-pict)
    (yblank tiny-y-sep)
    (word-append
      @bodyrmlo{Three common answers: } @bodyembf{yes} @bodyrmlo{, } @bodyembf{no} @bodyrmlo{, and } @bodyembf{sort of})
  )
  (pslide
    #:go hi-text-coord-m
    (what-do-types-mean)
    #:next
    (yblank small-y-sep)
    (hc-append
      small-x-sep
      (question-text
        (word-append
          @bodyrmlo{Did anyone } @bodyrmhi{ask} @bodyrmlo{ programmers?}))
      (three-answers-face))
    #:next
    (yblank tiny-y-sep)
    (ht-append (xblank big-x-sep) (challenge-pict @bodyrmlo{How to compare languages?}))
  )
  (pslide
    #:go (coord 1/2 92/100 'cb)
    (hrule (w%->pixels 7/10))
    (yblank tiny-y-sep)
    (tag-pict (all-lang-stripe) 'als)
    #:go slide-text-coord-m
    (challenge-pict @bodyrmlo{How to compare languages?})
    (yblank small-y-sep)
    #:alt ( (lang-vs-lang 0) )
    (tag-pict (lang-vs-lang 1) 'lvl)
    #:next
    #:go (at-find-pict 'lvl cc-find 'cc)
    (ppict-do
      (compare-semantics-pict)
      #:go (coord 1/2 1 'ct #:abs-y tiny-y-sep)
      (scale (bridge-pict2 (typed-icon) (untyped-icon)) 8/10))
    #:next
    #:go (at-find-pict 'als cc-find 'cc)
    (cc-superimpose
      (filled-rectangle
        (w%->pixels 95/100)
        (h%->pixels 28/100)
        #:draw-border? #f
        #:color white)
      (dls-lang-pict 2))
  )
  (pslide
    #:go heading-coord-m
    (dls-behavior-head 0)
    #:go (coord slide-text-right 15/100 'rt)
    (tag-pict
      (browncs-box
        (word-append
          @bodyrmlo{A } @bodyembf{method} @bodyrmlo{ to compare semantics}))
      'smethod)
    #:next
    #:go (coord 15/100 slide-text-top 'lt)
    (yblank tiny-y-sep)
    #:alt ( (bad-array-example-survey) )
    (vc-append
      pico-y-sep
      (bad-array-example-survey)
      (thick-down-arrow med-y-sep)
      (yblank pico-y-sep)
      (bad-array-example-survey-ans))
    #:next
    #:go (coord 85/100 hi-text 'rt)
    (vr-append
      (h%->pixels 20/100)
      (bghost @bodyrm{One program, up to 3 outputs})
      (ht-append (tag-pict (LE-matrix #:x '(face)) 'LE) (xblank tiny-x-sep)))
    #:go (at-find-pict 'LE lt-find 'rb #:abs-x (- pico-x-sep) #:abs-y (- pico-y-sep))
    (tag-pict (blank 12 12) 'tgt)
    #:set
    (let ((pp ppict-do-state))
      (add-code-arrows
        pp
        (code-arrow 'A0-E rc-find 'tgt rt-find (* 0 turn) (* 3/4 turn) 1/4 80/100 'solid)
        (code-arrow 'A1-E rc-find 'tgt cc-find (* 0 turn) (* 88/100 turn) 1/4 45/100 'solid)
        (code-arrow 'A2-E rc-find 'tgt lb-find (* 0 turn) (* 0 turn) 80/100 45/100 'solid)))
    #:next
    #:go (at-find-pict 'smethod rb-find 'rt)
    (vl-append
      2
      (arrow-bullet @bodyrmlo{One program})
      (arrow-bullet @bodyrmlo{Distinct results})
      (arrow-bullet @bodyrmlo{Task: Label each result}))
    )
  (pslide
    #:go heading-coord-m
    (dls-behavior-head 0)
    #:go heading-coord-m
    (bghost @headrm{S})
    (yblank small-y-sep #;(h%->pixels 7/100))
    (dls-pop-pict)
    #:next
    #:go (coord 1/2 88/100 'cb)
    #:alt ( 
            (dls-lang-pict)
            #:go center-coord
            (browncs-box
              (word-append
                @bodyrmlo{How do the responses relate to the 3 } @bodyembf{semantics} @bodyrmlo{?}))
            )
    (dls-lang-pict 1)
    #:go (at-find-pict 'Guarded ct-find 'cb #:abs-y (- tiny-y-sep))
    (dls-le-tower)
    #:go (at-find-pict 'Transient ct-find 'cb #:abs-y (- tiny-y-sep))
    (dls-du-tower)
    #:go (at-find-pict 'Erasure ct-find 'cb #:abs-y (- tiny-y-sep))
    (dls-du-tower)
    #:go (at-find-pict 'Guarded ct-find 'cb #:abs-y (- big-y-sep))
    (browncs-box @coderm{Expected & Like})
    #:go (at-find-pict 'Transient ct-find 'cb #:abs-y (- big-y-sep) #:abs-x big-x-sep)
    (browncs-box @coderm{Unexpected & Dislike})
  )
  (void))

(define (sec:case-closed)
  (pslide
    #:go heading-coord-m
    @headrm{Case Closed?}
    #:go (coord 1/2 88/100 'cb)
    (dls-lang-pict 1)
  )
  (pplay
    (lambda (pp n)
      (ppict-do
        pp
        #:go heading-coord-m
        @headrm{Case Closed?}
        #:go (coord 1/2 88/100 'cb)
        (dls-lang-pict 1)
        #:go heading-coord-m
        (bghost @headrm{C})
        (yblank tiny-y-sep)
        (cellophane (all-lang-stripe) (- 1 n))
        (yblank tiny-y-sep)
        (hrule (w%->pixels 7/10))
        (yblank pico-y-sep)
        (cellophane (arrange-lang-stripe) n)
    ))
  )
  (pslide
    #:go heading-coord-m
    @headrm{Case Closed? No!}
    #:go (coord 1/2 88/100 'cb)
    (dls-lang-pict 1)
    #:go heading-coord-m
    (bghost @headrm{C})
    (yblank tiny-y-sep)
    (bghost (all-lang-stripe))
    (yblank tiny-y-sep)
    (hrule (w%->pixels 7/10))
    (yblank pico-y-sep)
    (arrange-lang-stripe)
    #:go slide-text-coord-mid
    @bodyrmlo{Funny split ...}
    #:next
    #:go (coord 52/100 22/100 'ct)
    (aca-vs-ind)
  )
  (pslide
    #:go heading-coord-m
    @headrm{Case Closed? No!}
    #:go (coord 1/2 88/100 'cb)
    (dls-lang-pict 1)
    #:go heading-coord-m
    (bghost @headrm{C})
    (yblank tiny-y-sep)
    (bghost (all-lang-stripe))
    (yblank tiny-y-sep)
    (hrule (w%->pixels 7/10))
    (yblank pico-y-sep)
    (arrange-lang-stripe)
    #:next
    #:go slide-text-coord-m
    (yblank small-y-sep)
    (vl-append
      pico-y-sep
      (word-append
        @bodyrmlo{There are } @bodyemrm{two} @bodyrmlo{ problems:})
      (xindent
        (vl-append 4
          (arrow-bullet
          (word-append
            @bodyrmlo{How should gradual types } @bodyemrm{behave} @bodyrmlo{?}))
          (arrow-bullet
          (word-append
            @bodyrmlo{What do behaviors } @bodyemrm{cost} @bodyrmlo{?})))))
  )

  (void))

(define (sec:my-research)
  (pslide
    ;; - hat in the ring of new languages (implicit from layout, no need for bridge)
    ;; - theme of my research is building methods
    #:go heading-coord-mid (typed-and-untyped-pict #:x '(notext))
    #:next
    #:go (coord 1/2 32/100 'ct)
    @headrm{My Research Focus}
    (word-append
      @bodyrm{Developing } @bodyembf{methods} @bodyrm{ for gradual language design})
    (blank 0 tiny-y-sep)
    (hrule (w%->pixels 55/100) #:thickness 2)
    (blank 0 small-y-sep)
    ;; focus on 1 2, mention 3
    ;; ? mention 1/2 = deep + shallow lang?
    (triangle-3p #:num #f #:bold '(1 2 3))
    )
  (void))

(define (sec:design-space)
;  (pslide
;    #:go slide-text-coord-mid
;    (triangle-3p #:num #f #:bold '(1))
;    (yblank small-x-sep)
;    (question-pict @bodyrm{What guarantees do types provide?})
;    )
;  (pslide
;    #:go (coord 1/2 92/100 'cb)
;    (hrule (w%->pixels 7/10))
;    (yblank tiny-y-sep)
;    (all-lang-stripe)
;    #:next
;    #:go (coord 1/2 slide-text-top 'ct)
;    #:alt (
;      (basic-example 'T #f)
;      #:next
;      (yblank small-y-sep)
;      (question-pict (word-append @bodyrmlo{Is } @bodybf{n} @bodyrmlo{ really a number?}))
;    )
;    (basic-example 'T 'U)
;    (yblank small-y-sep)
;    (question-pict (word-append @bodyrmlo{Is } @bodybf{n} @bodyrmlo{ really a number?}))
;    #:next
;    (yblank tiny-y-sep)
;    (word-append
;      @bodyrmlo{Some say } @bodyembf{yes} @bodyrmlo{, others say } @bodyembf{no})
;  )
;  (pslide
;    #:go heading-coord-mid
;    @headrm{Example: Function Type}
;    #:go (coord 38/100 slide-text-top 'ct)
;    (tag-pict (bad-fun-example 'U 'T 'U) 'ex)
;    #:next
;    #:go (at-find-pict 'ex rb-find 'ct #:abs-x (- med-x-sep) #:abs-y (- tiny-y-sep))
;    (question-pict
;      @bodyrm{Can types detect a bad function?})
;    #:next
;    (yblank small-x-sep)
;    (yes-no-pyramid
;      bad-fun-example-yes-lang*
;      bad-fun-example-no-lang*)
;    )
;  (pslide
;    #:go heading-coord-mid
;    variety-head-1
;    (yblank pico-y-sep)
;    @headrm{Many Languages, Many Semantics}
;    #:go variety-coord-1
;    variety-lang-1
;    variety-text-1
;    #:go variety-coord-2
;    variety-lang-2
;    variety-text-2
;    #:go variety-coord-3
;    variety-lang-3
;    variety-text-3
;    #:next
;    #:go (at-find-pict 'N-lang #:abs-y tiny-y-sep) (question-box @bname{Guarded})
;    #:go (at-find-pict 'T-lang #:abs-y tiny-y-sep) (question-box @bname{Transient})
;    #:go (at-find-pict 'E-lang #:abs-y tiny-y-sep) (question-box @bname{Erasure})
;    )
  (pslide
    #:go heading-coord-mid
    @headrm{A Formal Characterization?}
    #:go hi-text-coord-mid
    #:alt ((property-table #:hide? #t design-pre-bg-table* #:num-cols 4))
    (property-table #:hide? #f design-pre-bg-table* #:num-cols 4)
    (yblank small-y-sep)
    (word-append
      @bodyrmhi{Standard tools } @bodyembf{do not} @bodyrmhi{ tell the difference!})
    )
  (pslide
    #:go heading-coord-mid
    @headrm{Example: Clickable Plot}
    ;; so far, small examples ... lets see a realistic one
    (yblank pico-y-sep)
    (word-append
      @bodyrm{Type Soundness cannot distinguish } @bname{Guarded} @bodyrm{ and } @bname{Transient})
    ;; so far, small examples ... lets see a realistic one
    (yblank med-y-sep)
    #:alt ((ht-append small-y-sep (click-plot-pict 0) click-plot-steps-pict))
    #:alt ((ht-append small-y-sep (click-plot-pict 1) click-plot-steps-pict))
    #:alt ((ht-append small-y-sep (click-plot-pict 2) click-plot-steps-pict))
    #:next
    ;;  staging ... bold etc.
    #:alt ((click-plot-example 0))
    (click-plot-example 1)
    #:next
    #:go bottom-coord-mid
    (browncs-box
      (l-line-append
        #:sep pico-y-sep
        (word-append
          @bname{Guarded} @bodyrm{: error at the } @bodybf{type boundary})
        @bodyrmlo{    (coordinate pair vs. mouse event)}
        (yblank small-y-sep)
        (word-append
          @bname{Transient} @bodyrm{: error } @bodybf{within} @bodyrm{ the client})
        (word-append
          @bodyrmlo{    the real issue is } @bodyrm{off the stack} @bodyrmlo{!})))
    )
  (pslide
    #:go heading-coord-mid
    @headrm{Example: Clickable Plot}
    ;; so far, small examples ... lets see a realistic one
    (yblank pico-y-sep)
    (word-append
      @bodyrm{Type Soundness cannot distinguish } @bname{Guarded} @bodyrm{ and } @bname{Transient})
    ;; so far, small examples ... lets see a realistic one
    (yblank med-y-sep)
    #:alt ((abstract-clickplot 0))
    #:alt ((abstract-clickplot 1))
    (abstract-clickplot 2)
    (yblank med-y-sep)
    (question-pict
      (word-append @bodyrm{Do types protect the } @bodybf{callback} @bodyrm{ channel?}))
    (yblank tiny-y-sep)
    #:next
    ;;  common problem but very different! with implications
    ;;  N types global, T types local
    (browncs-box
      (l-line-append
        #:sep pico-y-sep
        (word-append @bname{Guarded} @bodyrm{: Yes})
        @bodyrmlo{    types made the channel}
        (yblank pico-y-sep)
        (word-append @bname{Transient} @bodyrm{: No})
        @bodyrmlo{    the channel is untyped to untyped}
        ))
    )
  (pslide
    #:go heading-coord-mid
    @headrm{A Toolbox to Measure Type Guarantees}
    #:go hi-text-coord-mid
    #;(property-table #:hide? #f design-m11-bg-table*)
    #:alt ((property-table #:row 0 #:hide? #t design-m22-bg-table*))
    #:alt (
      (property-table #:row 1 #:hide? #f design-m22-bg-table*)
      (yblank small-y-sep)
      (cm-desc-pict)
    )
    #:alt (
      (property-table #:hide? #f design-m22-bg-table*)
      (yblank small-y-sep)
      (cm-desc-pict)
      (yblank tiny-y-sep)
      (bs-desc-pict)
      (yblank tiny-y-sep)
      (bc-desc-pict)
    )
    #:alt ((property-table #:hide? #t #:x-margin pico-x-sep design-post-bg-table*))
    (property-table #:hide? #f #:x-margin pico-x-sep design-post-bg-table*)
    )
  (pslide
    #:go heading-coord-left
    @headrm{In Summary}
    #:go slide-text-coord-mid
    (ppict-do
      (triangle-3p #:num #f #:bold '(1))
      #:go (at-find-pict 't-proofs rb-find 'rt #:abs-x (- (w%->pixels 12/100)) #:abs-y (- pico-y-sep))
      (design-tools-pict 150))
    #:next
    #:go (coord slide-text-left 42/100 'lt)
    (summary-table
      @bodyrm{Many behaviors for gradual types}
      @bodyrm{How to compare semantics?}
      @bodyrm{Metatheory Toolbox, Semantic Framework})
    )
  ;; PS: implications for TYPED languages too
  ;; many t lang trust untyped code,
  ;;  gt makes boundaries visible
  ;;  and challenges notions of types and what they mean
  ;;
  ;; cm strengthens ts for programs that mix t and u code
  ;;  and enables precise statements about the quality of blame

  (void))

(define (sec:perf)
  (pslide
    #:go heading-coord-l
    @headrm{Where Do Costs Come From?}
    #:next
    #:go (coord 15/100 slide-text-top 'lt)
    (yblank tiny-y-sep)
    (dls-array-nt)
    #:next
    #:go (coord 50/100 36/100 'lt)
    (dls-detect-pict)
  )
  (pslide
    #:go heading-coord-r
    (ht-append tiny-x-sep @headrm{Caution: Typed Racket} (tr-pict))
    #:go hi-text-coord-m
    (word-append
      @bodyembf{Guarded} @bodyrmlo{ type guarantees, but }
      @bodyrmhi{huge} @bodyrmlo{ worst-case costs})
    (yblank small-y-sep)
    (ht-append
      small-x-sep
      ;; from Collapsible Contracts
      (perf-box 25) (perf-box 180) (perf-box 1400))
  )
  (pslide
    ;(browncs-box
    ;  @bodyrm{``[TR] seemed to combine the best of both worlds .... but in practice seem to combine mainly the downsides''})
    #:go (coord 1/2 92/100 'cb)
    (question-text @bodyrmlo{Are bad points common, or rare?})
    (yblank small-y-sep)
    (browncs-box
      (vc-append
        tiny-y-sep
          (word-append
            @bodyrmlo{Need a } @bodyembf{method} @bodyrmlo{ to measure performance})
        (PPP-pict 1)))
    (yblank big-y-sep)
    (hrule (w%->pixels 7/10))
    (yblank tiny-y-sep)
    (all-lang-stripe)
  )
  (pslide
    #:go heading-coord-left
    @headrm{One Program, Many Points}
    #:go slide-text-coord-mid
    (word-append
      @bodyrmlo{What to Measure} @bodyrmlo{ = } @bodyrmhi{All} @bodyrmlo{ Gradual Possibilities})
    #:next
    (yblank small-y-sep)
    (scale the-mixed-pict 7/10)
    (yblank tiny-y-sep)
    (word-append
      @bodyrmlo{One program with } @bodyrmhi{5} @bodyrmlo{ components ...})
    #:next
    (yblank tiny-y-sep)
    (thick-down-arrow (h%->pixels 6/100))
    (yblank small-y-sep)
    (tag-pict (scale-to-width% example-lattice-5 92/100) 'big-lattice)
    (yblank tiny-y-sep)
    (word-append
      @bodyrmlo{... leads to } @bodyrmhi{32} @bodyrmlo{ gradual points})
    (yblank small-y-sep)
    (word-append
      @bodyrmlo{In general, } @bodyrmhi{N}
      @bodyrmlo{ components  =>  }
      @bodyrmhi{2^N}
      @bodyrmlo{ points})
    ;; max = 2**17 = 131k
    #:next
    #:go (at-find-pict 'big-lattice cc-find 'lc #:abs-x tiny-x-sep #:abs-y (- tiny-y-sep))
    (challenge-pict
      @bodyrmlo{How to analyze the data?})
    )
  (pslide
    #:go heading-coord-r @headrm{Performance Insight}
    #:go slide-text-coord-mid
    (challenge-pict
      @bodyrmlo{How to analyze the data?})
    (yblank small-y-sep)
    (word-append
      @bodyrmlo{Focus on  } @bodyembf{D} @bodyrmhi{-deliverable} @bodyrmlo{  configurations})
    )
  (pslide
    #:go heading-coord-left
    (dd-head 0 @headrm{The Idea})
    #:go (coord 1/2 hi-text 'ct)
    (ht-append
      small-x-sep
      (vc-append
        tiny-y-sep
        (d-deliv-icon 200)
        (word-append
          @bodyrmlo{Are we  } @bodyitbf{fast enough} @bodyrmlo{?}))
      (migration-append
        (path-node '(T U T T T))
        (path-node '(T U T T U))
        (path-node '(T U U U U))
        (path-node '(U U U U U))))
    #:next
    (yblank small-y-sep)
    (question-box
      (m-line-append
        (word-append
          @bodyrmhi{Worst-case} @bodyrmlo{ overhead is } @bodyrmhi{not} @bodyrmlo{ important})
        (word-append
          @bodyembf{D} @bodyrmhi{x} @bodyrmlo{ slower is the upper bound})))
    )
  (pslide
    #:go heading-coord-left
    (dd-head 1 @headrm{How to Use})
    #:go hi-text-coord-mid
    (scale-to-width% example-lattice-5 92/100)
    (yblank tiny-y-sep)
    (hrule (w%->pixels 50/100))
    #:next
    (yblank small-y-sep)
    #:alt ( (ht-append tiny-x-sep D-use-l (bghost D-use-r)) )
    (ht-append tiny-x-sep D-use-l D-use-r)
    )
  (pslide
    #:go heading-coord-left
    (dd-head 2 @headrm{How to Scale})
    #:go hi-text-coord-mid
    (tag-pict (bghost (scale-to-width% example-lattice-5 92/100)) 'lattice)
    #:go (at-find-pict 'lattice cb-find 'cb)
    (add-rectangle-background
      (scale-to-fit (example-lattice-n 6) (* client-w 3) client-h))
    #:go (at-find-pict 'lattice cb-find 'ct)
    (yblank tiny-y-sep)
    (hrule (w%->pixels 50/100))
    #:next
    (yblank small-y-sep)
    (word-append
      @bodyrmlo{Choosing } @bodyembf{D} @bodyrmlo{ enables a }
      @bodyrmhi{Bernoulli random variable})
    #:alt (
      (yblank small-y-sep)
      (lattice-to-y/n-pict)
      (yblank small-y-sep)
      #:next
      (word-append
        @bodyrmlo{If 50% of } @bodyrmhi{all points}
        @bodyrmlo{ are } @bodyembf{D} @bodyrmlo{-deliverable})
      @bodyrmlo{=>}
      (word-append
        @bodyrmlo{A } @bodyrmhi{random point} @bodyrmlo{ has a 50% chance of being fast enough})
    )
    #:go (coord 1/2 86/100 'cb)
    (linear-sampling-works)
  )
  (pslide
    #:go heading-coord-left @headrm{Method}
    #:go slide-text-coord-mid
    (yblank tiny-y-sep)
    (perf-method-pict 1)
    (yblank small-y-sep)
    (let ((pp (bitmap "img/overhead3.png")
            #;(scale-to-width%
              (ht-append pico-x-sep
                (big-overhead-pict '(spectralnorm) 'S #:legend #f)
                (big-overhead-pict '(jpeg) 'D #:legend #f)
                (big-overhead-pict '(pystone) 'S #:legend #f))
              90/100)
          ))
      #;(save-pict "bgtmp.png" pp)
      (vr-append
        tiny-y-sep
        pp
        @bodyrmlo{Larger Area = Better Performance  }))
    )

  (void))

(define (sec:both)
  (pslide
    #:go heading-coord-mid
    @headrm{Applications}
    (yblank tiny-y-sep)
    (tag-pict (bghost @bodyrmlo{C}) 'curated)
    #:go center-coord
    (tag-pict (vrule (h%->pixels 5/10)) 'thebar)
    #:alt (
      #:go (coord 47/100 hi-text 'rt)
      (vl-append
        tiny-y-sep
        (tr-app-head)
        (xblank (pict-width (tr-app-body))))
      #:go (coord 53/100 hi-text 'lt)
      (rp-app-head)
    )
    #:alt (
      #:go (at-find-pict 'curated)
      @bodyrmlo{Curated benchmarks for two languages}
      #:go (coord 47/100 hi-text 'rt)
      (vl-append
        tiny-y-sep
        (tr-app-head)
        (xblank (pict-width (tr-app-body))))
      (benchmark-pict3
        "img/gtp-bench.png"
        #:w% 41/100
        #:url "docs.racket-lang.org/gtp-benchmarks")
      #:go (coord 53/100 hi-text 'lt)
      (vl-append
        tiny-y-sep
        (rp-app-head)
        (xindent
          (benchmark-pict3
            "img/retic-bench.png"
            #:w% 30/100
            #:url "nuprl.github.io/gtp/benchmarks")))
      #:go (at-find-pict 'thebar cb-find 'ct #:abs-y (+ tiny-y-sep small-y-sep))
      @bodyrmlo{from GitHub, Racket packages, Python benchmarks, ... usually without types}
    )
    #:alt (
      #:go (coord 53/100 hi-text 'lt)
      (rp-app-head)
      #:go (coord 47/100 hi-text 'rt)
      (vl-append
        tiny-y-sep
        (tr-app-head)
        (tr-app-body))
      #:next
      (yblank tiny-y-sep)
      (ht-append (tr-improvement-pict2) (xblank tiny-x-sep))
    )
    #:go (coord 47/100 hi-text 'rt)
    (vl-append tiny-y-sep (tr-app-head) (tr-app-body))
    #:go (coord 53/100 hi-text 'lt)
    (vl-append tiny-y-sep (rp-app-head) (rp-app-body))
    #:next
    #:go (at-find-pict 'thebar cb-find 'ct #:abs-y (- big-y-sep))
    (nt-equal-ts-pict)
    #:next
    (yblank tiny-y-sep)
    (browncs-box
      (vc-append
        tiny-y-sep
          (word-append
            @bodyrmlo{Need a } @bodyembf{method} @bodyrmlo{ to assess type guarantees})
        (PPP-pict 2)))
  )
  (pslide
    #:go heading-coord-m
    (nt-equal-ts-pict)
    #:next
    (yblank tiny-y-sep)
    (word-append
      @bodyrmhi{Type Soundness} @bodyrmlo{ (TS) is the standard property for typed languages})
    (yblank pico-y-sep)
    (word-append
      @bodyrmhi{"typed code agrees with the types"})
    #:next
    (yblank tiny-y-sep)
    #:alt ( (survey-says 0) )
    #:alt ( (survey-says 1) )
    (survey-says 2)
  )
  (pslide
    #:go heading-coord-l
    (add-venue-head
      @headrm{From  TS  to  CM}
      (ht-append pico-x-sep (venue->pict "OOPSLA'19") (venue->pict "In Submission'22")))
    #:go heading-coord-m
    (bghost @headrm{M})
    (yblank tiny-y-sep)
    (word-append
      @bodyrmlo{Both } @bodyembf{Guarded} @bodyrmlo{ and }
      @bodyembf{Transient} @bodyrmlo{ satisfy } @bodyrmhi{type soundness} @bodyrmlo{ (TS)})
    (yblank 2)
    (word-append
      @bodyrmlo{Only } @bodyembf{Guarded}
      @bodyrmlo{ satisfies } @bodyrmhi{complete monitoring} @bodyrmlo{ (CM)})
    #:next
    #:go (coord 7/100 hi-text 'lt)
    (yblank med-y-sep)
    #:alt ( (bad-array-example-survey2) )
    (vc-append
      pico-y-sep
      (bad-array-example-survey2)
      (thick-down-arrow med-y-sep)
      (yblank pico-y-sep)
      (bad-array-example-survey-ans2 #:lbl '(A0 A1)))
    #:next
    #:go (coord (- 1/2 9/100) hi-text 'lt)
    (yblank big-y-sep)
    #:alt ( (scale (abstract-clickplot2 1) 8/10) )
    (scale (abstract-clickplot2 2) 8/10)
    #:next
    (yblank (h%->pixels 8/100))
    (tag-pict
      (question-pict
        (word-append
          @bodyrmlo{Do types protect the } @bodyemrm{derived} @bodyrmlo{ channel?}))
      'qq)
    #:next
    #:go (at-find-pict 'qq cb-find 'ct #:abs-y tiny-y-sep)
    (browncs-box
      (l-line-append
        #:sep pico-y-sep
        (vl-append 2
          (word-append @bodyembf{Guarded} @bodyrmlo{ (CM+TS): } @bodyrmhi{Yes})
          @bodyrmlo{    types made the channel})
        (vl-append 2
          (word-append @bodyembf{Transient} @bodyrmlo{ (TS): } @bodyrmhi{No})
          @bodyrmlo{    channel is untyped to untyped})))
  )
  (pslide
    #:go heading-coord-mid
    @headrm{Applications}
    #:go center-coord
    (tag-pict (vrule (h%->pixels 5/10)) 'thebar)
    #:go (coord 47/100 hi-text 'rt)
    (vl-append tiny-y-sep (tr-app-head 1) (xblank (pict-width (tr-app-body))))
    #:go (coord 53/100 hi-text 'lt)
    (rp-app-head 1)
    #:next
    #:go (at-find-pict 'thebar ct-find 'ct #:abs-y big-y-sep)
    (browncs-box
      (question-text
        (word-append
          @bodyrmlo{Are } @bodyembf{Guarded} @bodyrmlo{ and } @bodyembf{Transient} @bodyrmlo{ types equally } @bodyitbf{strong} @bodyrmlo{?})))
    #:next
    (yblank tiny-y-sep)
    (browncs-box @bodyrmlo{No!})
    #:next
    (yblank small-y-sep)
    (challenge-pict
      (word-append
        @bodyrmlo{Can the two interoperate?}))
    #:next
    (yblank tiny-y-sep)
    (browncs-box
      (vc-append
        tiny-y-sep
        (word-append
          @bodyrmlo{Yes, } @bodyembf{Deep} @bodyrmlo{+} @bodyembf{Shallow} @bodyrmlo{ Racket})
        (PPP-venue "PLDI'22" "") ))
  )

  (pslide
    #:go heading-coord-mid
    @headrm{Foundations for Gradual Languages}
    (yblank tiny-y-sep)
    (tu-icon)
    (yblank small-y-sep)
    (invert-triangle-3p #:num #f #:bold '() #:hide '())
    (yblank small-y-sep) (hrule (w%->pixels 5/10)) (yblank small-y-sep)
    #:alt ( (all-lang-stripe) )
    (above-all-lang
      (research-contrib-summary))
  )
  (pslide
    #:go bottom-coord-mid
    (invert-triangle-3p #:num #f #:bold '(3) #:hide '())
    #:go heading-coord-l
    @headrm{Ongoing Work}
    #:go (coord 1/2 18/100 'ct)
    #:alt ( (people-bar 0) #:next #:go people-coord (sp-explained) )
    #:alt ( (people-bar 1) #:go people-coord (rp-explained) )
    (people-bar 2) #:go people-coord (fm-explained)
  )
  (sec:ltl)
  (pslide
    #:go bottom-coord-mid
    (invert-triangle-3p #:num #f #:bold '(3) #:hide '())
    #:go heading-coord-l
    @headrm{Ongoing Work}
    #:go (coord 1/2 18/100 'ct)
    (people-bar 3)
  )

  (void))

(define (sec:non-gt)
  (sec:ltl)
  (sec:alloy)
  (void))

(define (sec:ltl)
  (pslide
    #:go heading-coord-l
    (vc-append
      pico-y-sep
      @headrm{LTL Misconceptions}
      @bodyrm{Linear Temporal Logic})
    #:go slide-text-coord-m
    (yblank (* 2 tiny-y-sep))
    (word-append
      @bodyrmlo{used in: }
      @bodyrmhi{verification}
      @bodyrmlo{, }
      @bodyrmhi{synthesis}
      @bodyrmlo{, and }
      @bodyrmhi{robot planning})
    #:next
    (yblank tiny-y-sep)
    #:alt ( (ltl-traffic-light #:ans #f) )
    (ltl-traffic-light #:ans #t)
    #:next
    (yblank small-y-sep)
    (yblank tiny-y-sep)
    (question-text
      (word-append
        @bodyembf{In what ways}
        @bodyrm{ is LTL tricky, and }
        @bodyembf{what can we do}
        @bodyrm{ about it?}))
    (yblank pico-y-sep)
    @bodyrmlo{Studies with researchers & students}
    #:next
    (yblank pico-y-sep)
    (browncs-box
      (word-append
        @bodyrmlo{Early outcome: } @bodyembf{Better syntax} @bodyrmlo{ for Alloy 6}))
  )
  (void))

(define (sec:alloy)
  (pslide
    #:go heading-coord-m
    @headrm{Teaching Alloy}
    #:go slide-text-coord-m
    (word-append
      @bodyrmhi{Alloy} @bodyrmlo{ is a } @bodyrmhi{modeling language}
      @bodyrmlo{ that comes with } @bodyrmhi{two styles} @bodyrmlo{:})
    (yblank (h%->pixels 6/100))
    #:next
    (ht-append
      med-x-sep
      (label-above
        (alloy-box (f-alloy-code))
        @bodyrmhi{Predicate})
      (label-below
        (label-above
          (alloy-box (r-alloy-code))
          @bodyrmhi{Relational})
        (yblank pico-y-sep)
        (word-append
          @bodyrmlo{(} @bodyrm{f} @bodyrmlo{ is transitive)})))
    #:next
    (yblank small-y-sep)
    (word-append
      @bodyembf{Problem} @bodyrmlo{: }
      @bodyrmhi{errors} @bodyrmlo{ assume you know both styles!})
    #:next
    (yblank (+ small-y-sep tiny-y-sep))
    (question-text
      (word-append
        @bodyrm{Can } @bodyembf{language levels}
        @bodyrm{ give a smooth introduction?}))
    (yblank tiny-y-sep)
    (halloy-tower)
  )
  (void))

(define (sec:alloy2)
  ;; no staging
  (pslide
    #:go heading-coord-m
    @headrm{Teaching Alloy}
    #:go slide-text-coord-m
    (word-append
      @bodyrmhi{Alloy} @bodyrmlo{ is a } @bodyrmhi{modeling language}
      @bodyrmlo{ that comes with } @bodyrmhi{two styles} @bodyrmlo{:})
    (yblank (h%->pixels 6/100))
    (ht-append
      med-x-sep
      (label-above
        (alloy-box (f-alloy-code))
        @bodyrmhi{Predicate})
      (label-below
        (label-above
          (alloy-box (r-alloy-code))
          @bodyrmhi{Relational})
        (yblank pico-y-sep)
        (word-append
          @bodyrmlo{(} @bodyrm{f} @bodyrmlo{ is transitive)})))
    (yblank small-y-sep)
    (word-append
      @bodyembf{Problem} @bodyrmlo{: }
      @bodyrmhi{errors} @bodyrmlo{ assume you know both styles!})
    (yblank (+ small-y-sep tiny-y-sep))
    (question-text
      (word-append
        @bodyrm{Can } @bodyembf{language levels}
        @bodyrm{ give a smooth introduction?}))
    (yblank tiny-y-sep)
    (halloy-tower)
  )
  (void))

(define (sec:end)
  (pslide
    #:go heading-coord-m
    @headrm{Future Work}
    #:next
    #:go hi-text-coord-mid
    (yblank small-y-sep)
    (word-append
      @bodyrmlo{Typed + Untyped}
      @bodyrmlo{ is a }
      @bodyemrm{multi-language}
      @bodyrmlo{ problem})
    (yblank small-y-sep)
    (bridge-pict3 @headrm{L0} @headrm{L1})
    (yblank small-y-sep)
    (vl-append
      tiny-y-sep
      (arrow-bullet @bodyrmlo{2 similar languages})
      (arrow-bullet
        (word-append @bodyrmlo{higher-order} @bodyrmlo{ interoperability}))
      (arrow-bullet
        (word-append
          @bodyemrm{strong}
          @bodyrmlo{ vs. }
          @bodyemrm{weak}
          @bodyrmlo{ invariants})))
  )
  (pslide
    #:go heading-coord-m
    @headrm{Future Work}
    (yblank small-y-sep)
    (word-append
      @bodyemrm{Multi-language systems} @bodyrmlo{ are everywhere!})
    #:next
    #:go (coord 10/100 hi-text 'lt)
    (yblank small-y-sep)
    (mixed-real-life)
    #:next
    #:go (coord 86/100 hi-text 'rt)
    (yblank small-y-sep)
    (mixed-future)
    #:next
    #:go (coord 1/2 90/100 'cb)
    ;; #:alt ( (balanced-foundation 0) )
    (balanced-foundation 1)
  )
  (pslide)
  (pslide-the-summary 1)

  (void))

(define (pslide-the-summary [n 0])
  (pslide
    #:go (coord 20/100 (+ 18/100 slide-text-top) 'cc)
    (faded-triangle)
    #:go (coord 12/100 slide-text-top 'lt)
    (summary-3p)
    #:go (coord 1/2 76/100 'ct)
    (if (< 0 n)
      (vc-append
        tiny-x-sep
        (hrule (w%->pixels 4/10))
      (hc-append
        small-x-sep
        (hc-append
          tiny-x-sep
          (blank) #;(tiny-triangle)
          (word-append
            @bodyrm{Methods for } @bodyembf{multi-language systems}))
        (let* ((pp (mixed-real-life #:lbl #f)
                #;(bridge-pict2 (typed-icon2 (blank)) (untyped-icon2 (blank))))
               (pp
                 (scale pp 55/100)
                 #;(LR-stack #:w% 8/10 #:h% 5/10 pp pp)))
          pp)))
      (blank))
  )
  (void))

(define (big-grid path**)
    (apply
      ht-append
      small-x-sep
      (for/list ((path* (in-list path**)))
        (browncs-box (apply vc-append (map bitmap path*))))))

(define (sec:qa)
  (pslide
    #:go heading-coord-mid
    (headrm "Informal Landscape")
    (yblank small-y-sep)
    (browncs-box (scale-to-fit (bitmap "img/gt-landscape.png") (* 8/10 client-w) client-h))
  )
  (pslide
    #:go heading-coord-mid
    (headrm "Deep + Shallow")
    (yblank small-y-sep)
    (browncs-box (scale-to-fit (bitmap "img/bg-ds.png") (* 5/10 client-w) client-h))
    (yblank tiny-y-sep)
    (word-append
      @bodyrm{Percent of gradual points that run fastest with a }
      @bodyemrm{Deep}
      @bodyrm{+}
      @bodyemrm{Shallow}
      @bodyrm{ mix})
  )
  (for ((pp** (in-list
                (list
                  '(("img/bg-perf-1.3.png" "img/bg-perf-2.1.png")
                    ("img/bg-perf-3.4.png"))
                  '(("img/bg-perf-4.2.png" "img/bg-perf-5.2.png")
                    ("img/bg-perf-6.4.png")
                    #;("img/bg-perf-7.1.png")
                    ))))
        (nn (in-naturals 1)))
    (pslide
      #:go heading-coord-mid
      (headrm (format "Deep or Shallow (~a/2)" nn))
      (yblank small-y-sep)
      (big-grid pp**)))
  (pslide
    #:go heading-coord-mid
    @headrm{Prior Work}
    #:go hi-text-coord-mid
    #:alt ((property-table #:hide? #t design-pre-bg-table* #:num-cols 4))
    (property-table #:hide? #f design-pre-bg-table* #:num-cols 4)
    (yblank small-y-sep)
    (word-append
      @bodyrmhi{Standard tools } @bodyembf{do not} @bodyrmhi{ tell the difference!})
  )
  (pslide
    #:go heading-coord-mid
    @headrm{A Toolbox to Measure Type Guarantees}
    #:go hi-text-coord-mid
    #;(property-table #:hide? #f design-m11-bg-table*)
    #:alt ((property-table #:row 0 #:hide? #t design-m22-bg-table*))
    #:alt (
      (property-table #:row 1 #:hide? #f design-m22-bg-table*)
      (yblank small-y-sep)
      (cm-desc-pict)
    )
    #:alt (
      (property-table #:hide? #f design-m22-bg-table*)
      (yblank small-y-sep)
      (cm-desc-pict)
      (yblank tiny-y-sep)
      (bs-desc-pict)
      (yblank tiny-y-sep)
      (bc-desc-pict)
    )
    #:alt ((property-table #:hide? #t #:x-margin pico-x-sep design-post-bg-table*))
    (property-table #:hide? #f #:x-margin pico-x-sep design-post-bg-table*)
  )
  (pslide
    #:go heading-coord-mid
    @headrm{Example: Clickable Plot}
    (yblank pico-y-sep)
    (word-append
      @bodyrm{Type Soundness cannot distinguish } @bname{Guarded} @bodyrm{ and } @bname{Transient})
    (yblank med-y-sep)
    #:alt ((ht-append small-y-sep (click-plot-pict 0) click-plot-steps-pict))
    #:alt ((ht-append small-y-sep (click-plot-pict 1) click-plot-steps-pict))
    #:alt ((ht-append small-y-sep (click-plot-pict 2) click-plot-steps-pict))
    #:next
    #:alt ((click-plot-example 0))
    (click-plot-example 1)
    #:next
    #:go bottom-coord-mid
    (browncs-box
      (l-line-append
        #:sep pico-y-sep
        (word-append
          @bname{Guarded} @bodyrmlo{: error at the } @bodyrmhi{type boundary})
        @bodyrmlo{    (coordinate pair vs. mouse event)}
        (yblank small-y-sep)
        (word-append
          @bname{Transient} @bodyrmlo{: error } @bodyrmhi{within} @bodyrmlo{ the client})
        (word-append
          @bodyrmlo{    the real issue is } @bodyrmhi{off the stack} @bodyrmlo{!})))
  )
  (pslide
    #:go heading-coord-mid
    @headrm{Example: Clickable Plot}
    (yblank pico-y-sep)
    (word-append
      @bodyrmlo{Type Soundness cannot distinguish } @bname{Guarded} @bodyrmlo{ and } @bname{Transient})
    (yblank med-y-sep)
    (abstract-clickplot 2)
    (yblank med-y-sep)
    (question-pict
      (word-append @bodyrmlo{Do types protect the } @bodyrmhi{callback} @bodyrmlo{ channel?}))
    (yblank tiny-y-sep)
    (browncs-box
      (l-line-append
        #:sep pico-y-sep
        (word-append @bname{Guarded} @bodyrmhi{: Yes})
        @bodyrmlo{    types made the channel}
        (yblank pico-y-sep)
        (word-append @bname{Transient} @bodyrmhi{: No})
        @bodyrmlo{    the channel is untyped to untyped}
        ))
  )
;  (pslide
;    ;; future: U T ... T T (no!) ... ins sec ... 
;    ;; REALLY gotta think about content here!!!
;    #:go center-coord
;    (vc-append
;      big-y-sep
;      (bridge-pict "Untyped" "Typed")
;      (vc-append
;        tiny-y-sep
;        ;; worked on table types?!
;        ;; working on FM?
;        (bridge-pict "Insecure" "Secure")
;        (bridge-pict "Free" "Lifetime")))
;    )
;  ;; Q. People like python, which anyway has types. So why GT doesn't seem to have future.
;  ;;    - Focus on SECURITY for the answer, what are the new areas to explore?
;  ;;    - SP unhappy
;  ;; Q. where to find a real gradual language ... what's a ready example

  (void))

(define (sec:expr)
  ;; TODO clean up as QA slides
  (pslide
    #:go center-coord
    tsdthf-oopsla-2012
    vksb-dls-2014
    tfffgksst-snapl-2017
    svcb-snapl-2015
    #:go (coord 1/2 1 'cc)
    (cloud (w%->pixels 1) (h%->pixels 20/100) happy-cloud-color #:style '(wide square))
    )
  (pslide
    #:go heading-text-coord-left
    @headrm{My Work in Qualitative Methods}
    @bodyrm{What do people actually think?}
    ;; ... what do think, how interact, how do they find, ...golly
    ;; beware challenges from experienced people ... what to lookout for? know your stuff.
    (yblank small-y-sep)
    (table2
      #:row-sep small-y-sep
      #:col-sep small-x-sep
      (list
        dls-pict @bodyrm{Behavior of Gradual Types}
        trs-pict @bodyrm{Adoption in Typed Racket}
        ig-pict @bodyrm{Sound types at Instagram}))
    #:go bottom-coord-right
    @bodyrm{Other topics: LTL, Alloy, Types for Tables}
    )
  (pslide
    #:go heading-text-coord-right
    @headrm{The Rational Programmer}
    ;; TODO exmples of shitty transfers?
    ;; NOTE theory held up, but only barely!
    (yblank tiny-y-sep)
    @bodyrmlo{"Usability without users"}
    ;; ... what do think, how interact, how do they find, ...golly
    ;; beware challenges from experienced people ... what to lookout for? know your stuff.
    (yblank small-y-sep)
    (the-rational-programmer)
    #:go hi-text-coord-left
    (table2
      #:row-sep small-y-sep
      #:col-sep tiny-x-sep
      #:row-align lt-superimpose
      (list
        rat-idea-pict @bodyrm{Are blame errors accurate?}
        rat-gen-pict  @bodyrm{Mutate correct programs}
        rat-fix-pict  (vl-append tiny-y-sep
                        @bodyrm{Follow a repair strategy:}
                        @bodyrm{ - Apply the suggested fix}
                        @bodyrm{ - Apply a random fix})))
    #:go bottom-coord-mid
    (browncs-box
      @bodyrm{Takeaway: what YOU can expect from rational behavior})
    )
  (void))

;; =============================================================================

(module+ main
  (do-show))

;; =============================================================================

(module+ raco-pict (provide raco-pict)
         ;;(define client-w 984) (define client-h 728)
         (define client-w 1320) (define client-h 726)
         (define raco-pict
  (ppict-do
    (make-cs.brown.edu client-w client-h)
    #;(make-titlebg client-w client-h)

    #:go heading-coord-m
    (survey-says 2)


  )))
