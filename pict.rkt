#lang at-exp racket/base

(provide
  transient-rkt-version
  typed-codeblock
  untyped-codeblock
  fig:model-interaction-x
  fig:model-interaction-y
  fig:opt0
  fig:opt1
  fig:any-wrap
  fig:no-wrap
  fig:index-of
  fig:ds-example-x
  fig:ds-example-y

@; fig:shallow-cannot-express
@; fig:check-performance
@; fig:all-performance
@; fig:all-exact
@; fig:check-example
@; fig:underapprox-example
@; fig:all-blame
 )

(require
  (only-in "main.rkt" bm rnd benchmark-name* glob-first default-rkt-version transient-rkt-version stransient)
  (only-in math/statistics mean)
  (only-in racket/draw make-color)
  (only-in racket/file file->lines)
  (only-in racket/list flatten)
  (only-in racket/list take-right first second third fourth)
  (only-in racket/math exact-floor)
  (only-in scribble/base centered tabular hspace)
  gtp-plot/performance-info
  gtp-plot/plot
  gtp-plot/typed-racket-info
  pict ppict/2 pict-abbrevs
  racket/format
  racket/match
  racket/runtime-path
  racket/string
  "with-cache/with-cache.rkt")

;; -----------------------------------------------------------------------------

(define-runtime-path HERE ".")
(define data-dir (build-path HERE "data"))
(define cache-dir (build-path HERE "with-cache"))
(define blame "blame")

(define turn revolution)

(define code-x-sep 12)
(define code-y-sep 10)

(define caption-shim 8)
(define hshim 8)
(define vshim 6)

(struct code-arrow [src-tag src-find tgt-tag tgt-find start-angle end-angle start-pull end-pull style] #:transparent)

(define (hex-triplet->color% x)
  (define-values [r g b]
    (values (arithmetic-shift x -16)
            (bitwise-and #x0000ff (arithmetic-shift x -8))
            (bitwise-and #x0000ff x)))
  (make-color r g b))

(define black (hex-triplet->color% #x222222))
(define untyped-color (hex-triplet->color% #xffffff))
(define typed-color (hex-triplet->color% #xf3f3f3))
(define success-color "Forest Green")
(define error-color "Firebrick")

(define (code-arrowhead angle)
  (cellophane (colorize (arrowhead 12 angle) black) 0.7))

(define right-arrow
  (vl-append 6 (blank) (code-arrowhead 0)))

(define down-arrow
  (code-arrowhead (* 3/4 turn)))

(define code-text-style (cons "Inconsolata" 'modern))
(define code-text-size 11)

(define (code-text str [extra-style #f] #:size [size code-text-size])
  (define style (if extra-style (cons extra-style code-text-style) code-text-style))
  (text str style size))

(define (label-text str)
  (text str '() code-text-size))

(define (code-text* str*)
  (if (string? str*)
      (code-text str*)
      (let ((ex-height (pict-height (code-text "x"))))
        (apply vl-append (* 1/4 ex-height) (map code-text str*)))))

(define (success-text str)
  (colorize (plain-text str) success-color))

(define (error-text str)
  (colorize (plain-text str) error-color))

(define (plain-text str)
  (code-text str 'bold))

(define (add-code-arrow pp arrow
                        #:arrow-size [pre-arrow-size #f]
                        #:line-width [pre-line-width #f]
                        #:color [color "black"]
                        #:label [label (blank)]
                        #:x-adjust-label [x-label 0]
                        #:y-adjust-label [y-label 0]
                        #:hide? [hide? #false])
  (define line-width (or pre-line-width 2))
  (define arrow-size (or pre-arrow-size 6))
  (pin-arrow-line
    arrow-size pp
    (let ((src-tag (code-arrow-src-tag arrow)))
      (if (symbol? src-tag) (find-tag pp src-tag) src-tag))
    (code-arrow-src-find arrow)
    (let ((tgt-tag (code-arrow-tgt-tag arrow)))
      (if (symbol? tgt-tag) (find-tag pp tgt-tag) tgt-tag))
    (code-arrow-tgt-find arrow)
    #:line-width line-width
    #:label label
    #:x-adjust-label x-label
    #:y-adjust-label y-label
    #:hide-arrowhead? hide?
    #:style (code-arrow-style arrow)
    #:start-angle (code-arrow-start-angle arrow)
    #:end-angle (code-arrow-end-angle arrow)
    #:start-pull (code-arrow-start-pull arrow)
    #:end-pull (code-arrow-end-pull arrow)
    #:color color))

(define (add-code-line  pp arrow
                        #:arrow-size [pre-arrow-size #f]
                        #:line-width [pre-line-width #f]
                        #:color [color "black"]
                        #:label [label (blank)]
                        #:x-adjust-label [x-label 0]
                        #:y-adjust-label [y-label 0])
  (define line-width (or pre-line-width 2))
  (pin-line
    pp
    (let ((src-tag (code-arrow-src-tag arrow)))
      (if (symbol? src-tag) (find-tag pp src-tag) src-tag))
    (code-arrow-src-find arrow)
    (let ((tgt-tag (code-arrow-tgt-tag arrow)))
      (if (symbol? tgt-tag) (find-tag pp tgt-tag) tgt-tag))
    (code-arrow-tgt-find arrow)
    #:line-width line-width
    #:label label
    #:x-adjust-label x-label
    #:y-adjust-label y-label
    #:style (code-arrow-style arrow)
    #:start-angle (code-arrow-start-angle arrow)
    #:end-angle (code-arrow-end-angle arrow)
    #:start-pull (code-arrow-start-pull arrow)
    #:end-pull (code-arrow-end-pull arrow)
    #:color color))

(define (add-code-arrow* pp . arrow*)
  (for/fold ((pp pp))
            ((arrow (in-list arrow*)))
    (add-code-arrow pp arrow)))

(define (tag-append . x*)
  (string->symbol (string-join (map ~a x*) "-")))

(define (add-hubs pp tag)
  (define io-margin 8)
  (define node-padding 6)
  (define h-blank (blank 0 io-margin))
  (define v-blank (blank io-margin 0))
  (vc-append
    node-padding
    (tag-pict v-blank (tag-append tag 'N))
    (hc-append
      node-padding
      (tag-pict h-blank (tag-append tag 'W)) (tag-pict pp tag) (tag-pict h-blank (tag-append tag 'E)))
    (tag-pict v-blank (tag-append tag 'S))))

(define shim-sep 4)
(define output-x 30)
(define output-y 12)

(define (make-example-atom-pict t-str* u-str* s-str* d-str*)
  (define t-pict (typed-codeblock t-str*))
  (define u-pict (untyped-codeblock u-str*))
  (define s-pict (code-text (string-append "Shallow: " s-str*)))
  (define d-pict (hc-append (code-text "Deep: ") (error-text d-str*)))
  (define ur-pict (vc-append caption-shim u-pict (vc-append (* 1/2 caption-shim) s-pict d-pict)))
  (ht-append hshim t-pict right-arrow ur-pict))

(define (make-example-pair-pict u-str* t-str* r-str*)
  (define u-pict (untyped-codeblock u-str*))
  (define t-pict (typed-codeblock t-str*))
  (define r-pict (if r-str* (error-text r-str*) (blank)))
  (define tu-pict
    (let* ((t/u
            (vl-append
              u-pict
              (tag-pict (blank shim-sep shim-sep) 't-shim)
              (blank 0 output-y)
              (tag-pict (blank shim-sep shim-sep) 'u-shim)
              t-pict
              (hb-append (blank shim-sep output-y)
                         (tag-pict (blank output-x 0) 'u-out-0)))))
      (add-code-arrow
        t/u
        (code-arrow 't-shim rb-find 'u-shim rt-find (* 3/4 turn) (* 3/4 turn) 0 0 'solid))))
  (define r-out-arrow (code-arrow 'u-out-0 lb-find 'u-out-0 rb-find 0 0 0 0 'dot))
  (ppict-do
    (add-code-arrow tu-pict r-out-arrow)
    #:go (at-find-pict 'u-out-0 rc-find 'lc #:abs-x (* 2 shim-sep))
    r-pict))

(define (interleave a* b*)
  (cond
    [(null? a*)
     b*]
    [(null? b*)
     a*]
    [else
      (list* (car a*) (car b*) (interleave (cdr a*) (cdr b*)))]))

(define (make-example-table . pict*)
  (define title*
    (list (title-text "Flow") (title-text "Reticulated")
          (title-text "Typed Racket") (title-text "Nom")))
  (table 2 (interleave (take-right title* (length pict*)) pict*)
    lt-superimpose lt-superimpose 10 34))


(define (typed-codeblock str* #:width-str [width #f] #:lbl [lbl #false])
  (define the-code
    (add-rounded-border
     #:radius 8
     #:background-color typed-color
     #:frame-width 1
     #:frame-color black
     #:x-margin 10
     #:y-margin 8
     (let ((ct (code-text* str*)))
       (if width (vl-append (blank (pict-width (code-text width)) 0) ct) ct))))
  (if lbl (vr-append 2 lbl the-code) the-code))

(define (untyped-codeblock str*)
  (add-rounded-border
   #:radius 4
   #:background-color untyped-color
   #:frame-width 3
   #:frame-color black
   #:x-margin 20
   #:y-margin 14
   (code-text* str*)))

(define (make-2table kv**
                     #:col-sep [pre-col-sep #f]
                     #:row-sep [pre-row-sep #f]
                     #:col-align [col-align lc-superimpose]
                     #:row-align [row-align cc-superimpose])
  (define col-sep (or pre-col-sep 328/5))
  (define row-sep (or pre-row-sep 364/5))
  (table 2 (flatten kv**) col-align row-align col-sep row-sep))

(define fig:all-type
  (let ()
    (define all-type
      (string-split
       #<< eos
;; import `rest` assuming type is correct
(require/typed racket/list
   (rest (All (A) A)))

(define fake-str : String
   (inst rest String))
 
(string-length fake-str)
 eos
       "\n"))
    (typed-codeblock all-type #:lbl (label-text "Universal Type"))))

(define fig:occurrence-type
  ;; `ann` needed to eliminate an Intersection type
  (let ()
    (define occ-type
      (string-split 
       #<< eos
(require/typed racket/function
  (identity (-> Any Boolean : String)))
                     
(define x : Any 0)

(define fake-str : String
  (if (identity x)
      (ann x String)
      (error 'unreachable)))
 
(string-length fake-str)
 eos
       "\n"))
    (typed-codeblock occ-type #:lbl (label-text "Occurrence Type"))))


(define fig:shallow-cannot-express
  (make-2table
   #:row-sep 2 #:col-sep 20 #:row-align lt-superimpose #:col-align lt-superimpose
   (list fig:all-type fig:occurrence-type)))

(define max-page-width 960)
(define overhead-plot-y 130)
(define overhead-x-sep 8)
(define *overhead-y-sep* (make-parameter 10))
(define MAX-OVERHEAD 20)
(define plot-font-size 9)
(define title-text-size 12)
(define title-text-face "Liberation Serif")
(define sc-text-style (cons "Inconsolata" 'modern))
(define sc-text-size 14)

(define *my-grid-y* (make-parameter #f))

(define (sc-text str #:size [size code-text-size])
  ;; TODO actually sc, match the latex?
  (code-text str #:size size))

(define (te-mode-text str [size #f])
  (define tt (text str (cons 'bold sc-text-style) (+ 0 title-text-size)))
  (add-rounded-border
    #:frame-width 2 #:x-margin 10 #:y-margin 10
    tt))

(define (title-text str)
  (text str (cons 'bold title-text-face) plot-font-size))

(define (render-overhead-plot* tag f-render bm-name* cache-dir)
  (define num-cols 2)
  (parameterize ((*GRID-NUM-COLUMNS* num-cols)
                 (*GRID-X* (/ max-page-width num-cols))
                 (*GRID-X-SKIP* overhead-x-sep)
                 (*GRID-Y-SKIP* (*overhead-y-sep*))
                 (*OVERHEAD-LEGEND?* #f)
                 (*OVERHEAD-SHOW-RATIO* #f)
                 (*AUTO-POINT-ALPHA?* #f)
                 (*POINT-ALPHA* 0.4)
                 (*FONT-SIZE* plot-font-size)
                 (*OVERHEAD-LINE-WIDTH* 0.1))
    (define grid-y
      (or (*my-grid-y*)
          (let ((len (quotient (length bm-name*) num-cols)))
            (+ (* overhead-plot-y len)
               (* (*overhead-y-sep*) (- len 1))))))
    (define pp
      (let ()
        (define (render-thunk)
          (parameterize ([*GRID-Y* grid-y])
            (grid-plot f-render bm-name*)))
        (if cache-dir
            (parameterize ([*current-cache-directory* cache-dir]
                           [*current-cache-keys* (list (λ () bm-name*))])
              (with-cache (cachefile (string-append tag ".rktd"))
                render-thunk))
            (render-thunk))))
    pp))

(define (my-render bm-name+v* f-render)
  (define bm-name (car bm-name+v*))
  (define-values [v0 v1] (values (cadr bm-name+v*) (cddr bm-name+v*)))
  (define pi-0 (benchmark-name->performance-info bm-name v0 #:full-name? #t))
  (define pi-1 (benchmark-name->performance-info bm-name v1 #:full-name? #t))
  (define mo
    (if (eq? 'infer (*OVERHEAD-MAX*))
        (max (max-overhead pi-0) (max-overhead pi-1))
        MAX-OVERHEAD))
  (vl-append
   2
   (title-text (format "~a" bm-name))
   (parameterize ((*OVERHEAD-MAX* mo))
     (f-render (list pi-0 pi-1)))))

(define (render-relative-overhead-plot bm-name+v*)
  (my-render bm-name+v* overhead-plot))

(define (render-relative-exact-plot bm-name+v*)
  (parameterize ((*OVERHEAD-FREEZE-BODY* #true))
    (my-render bm-name+v* exact-runtime-plot)))

(define (benchmark-name->performance-info bm-name version #:full-name? [full-name? #f])
  (when (and (eq? bm-name 'zordoz)
             (string=? version "6.4"))
    (error 'die))
  (define data-dir (benchmark-name->data-file bm-name version))
  (define extra-name (and full-name? (string->symbol (format "~a-~a" bm-name version))))
  (make-typed-racket-info data-dir #:name extra-name))

(define (benchmark-name->data-file bm-name version)
  (define pp
    (let* ((name bm-name)
           (patt (format "~a-*rktd" name)))
      (glob-first (build-path data-dir version patt))))
  (if (file-exists? pp)
      pp
      (raise-argument-error 'benchmark-name->data-file "directory-exists?" pp)))

(define (render-exact2 cache-tag bm-name*)
  (let* ((bm+v* (for/list ((bm-name (in-list bm-name*)))
                  (cons bm-name (cons transient-rkt-version stransient)))))
    (render-overhead-plot* cache-tag render-relative-exact-plot bm+v* cache-dir)))

(define (render-overhead2 cache-tag bm-name*)
  (let* ((bm+v* (for/list ((bm-name (in-list bm-name*)))
                  (cons bm-name (cons transient-rkt-version stransient)))))
    (render-overhead-plot* cache-tag render-relative-overhead-plot bm+v* cache-dir)))

(define (fig:check-performance)
  (let* ((bm-name* '(dungeon jpeg take5 synth)))
    (render-overhead2 "check-performance" bm-name*)))

(define (fig:all-performance)
  (parameterize ((*my-grid-y* 1040)
                 (*overhead-y-sep* 1))
    (let* ((bm-name* benchmark-name*))
      (render-overhead2 "all-overhead" bm-name*))))

(define (fig:all-exact)
  (parameterize ((*my-grid-y* 1040)
                 (*overhead-y-sep* 1))
    (let* ((bm-name* benchmark-name*))
      (render-exact2 "all-exact" bm-name*))))

(define time-string->values
  (let ()
    (define TIME-RX #rx"^cpu time: ([0-9]+) real time: ([0-9]+) gc time: ([0-9]+)")
    (lambda (str)
      (define m (regexp-match TIME-RX str))
      (if m
          (values (string->number (cadr m))
                  (string->number (caddr m))
                  (string->number (cadddr m)))
          (raise-arguments-error 'time-string->values "param does not match 'time' regexp" "param" str "regexp" TIME-RX)))))

(define (time-string->cpu-time str)
  (let-values ([(cpu real gc) (time-string->values str)])
    cpu))

(define BLAME-TITLE
  (list "Benchmark"
        "Shallow worst-case"
        "Deep worst-case"
        "Shallow blame"))

(define (render-blame-table row*)
  (centered
   (tabular
    #:sep (hspace 3)
    #:style 'block
    #:row-properties '(bottom-border 1)
    #:column-properties '(left right)
    (list* BLAME-TITLE
           (map cdr row*)))))

(define (get-blame-table name*)
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () name*))])
    (with-cache (cachefile "blame-table.rktd")
      (λ ()
        (for/list ([name (in-list name*)])
          (make-blame-row name
                          (benchmark-name->blame-info name)
                          (benchmark-name->performance-info name stransient)
                          (benchmark-name->performance-info name transient-rkt-version)))))))

(define (make-blame-row name bd pi-shallow pi-deep)
  (list name
        (bm name)
        (rnd (max-overhead pi-shallow))
        (rnd (max-overhead pi-deep))
        (format-blame-data bd (performance-info->untyped-runtime pi-shallow))))

(define (benchmark-name->blame-info name [blame-dir blame])
  (define blame-file (build-path data-dir blame-dir (format "~a.rktd" name)))
  (define data-line*
    (let ((l* (file->lines blame-file)))
      (unless (string=? "#lang gtp-measure/output/file" (car l*))
        (raise-argument-error 'benchmark-name->blame-info "#lang gtp-measure file" blame-file))
      (cdr l*)))
  (cond
    [(and (null? (cdr data-line*))
          (string=? "timeout 666000" (car data-line*)))
     'timeout]
    [(and (null? (cdr data-line*))
          (regexp-match? #rx"^/bin/sh: line [0-9]: [0-9]+ Killed" (car data-line*)))
     'oom]
    [else
     (for/list ((dl (in-list data-line*)))
       (time-string->cpu-time dl))]))

(define (format-blame-data bd u-t)
  (define timeout-ms (* 10 ;; min
                        60 ;; min->sec
                        1000)) ;; sec->ms
  (case bd
    ((timeout) (format "TO (>~a)" (exact-floor (/ timeout-ms u-t))))
    ((oom) "OOM")
    (else (rnd (/ (mean bd) u-t)))))

(define (blame-data? x)
  (or (listof-real? x)
      (eq? x 'timeout)
      (eq? x 'oom)))

(define (listof-real? x)
  (and (list? x) (andmap real? x)))

(define blame-row-name cadr)

(define (blame-row-shallow r)
  (define str (caddr r))
  (string->number str))

(define (blame-row-deep r)
  (define str (caddr (cdr r)))
  (string->number str))

(define (blame-row-blame r)
  (define v (cadddr (cdr r)))
  (or (string->number v)
      v))

(define (newline-split str)
  (string-split str "\n"))

(define fig:check-example
  (let ()
    (define fn-decl    "(define (sum-list (nums : (Listof Real))) : Real")
    (define fn-check   "  (check! list? nums)")
    (define for-decl*  '("  (for/fold ([acc 0])"
                         "            ([n (in-list nums)])"))
    (define body*      '("    (+ acc n)))"))
    (define check-body*'("    (check! real? n)"
                         "    (+ acc n)))"))

    (define max-expanded "                     (rest (cdr lst))")
    
    (define s (label-text "Source code"))
    (define c (label-text "Source with checks"))
    (define e (label-text "Expanded code"))
    (define source   (typed-codeblock #:lbl s (append (list fn-decl) for-decl* body*)))
    (define checked  (typed-codeblock #:lbl c (append (list fn-decl fn-check) for-decl* check-body*)))

    (define expand-1
      (string-split
       #<< eos
(define-values [sum-list]
  (lambda (nums)
    (letrec-values 
      (((for-loop)
        (lambda (acc lst)
          (if (pair? lst)
              (let* ((n (car lst))
                     (rest (cdr lst))
                     (acc (+ acc n)))
                 (for-loop acc rest))
              acc))))
      (for-loop 0 nums))))
 eos
       "\n")
      )

    (define expand-old
      '("(define-values (sum-list)"
        " (lambda (nums)"
        "   (letrec-values "
        "      (((for-loop)"
        "       (lambda (acc lst)"
        "         (if (pair? lst)"
        "             (let* ((n (car lst))"
        "                    (rest (cdr lst))"
        "                    (acc (+ acc n)))"
        "                (for-loop acc rest))"
        "             acc))))"
        "     (for-loop 0 nums))))"))

    (define expanded (typed-codeblock #:lbl e #:width-str max-expanded expand-1))
    (ht-append 6
     (vl-append code-y-sep source checked)
     expanded)))

(define fig:underapprox-example
  (let ()
    (define src
      (typed-codeblock
        #:lbl (label-text "Source code")
        #:width-str "(define-values [pos->vals init val-cont? ....]"
        (newline-split
#<<eos
(for ((byte (open-input-string "boris")))
  (add1 byte))
eos
)))
    (define tgt
      (typed-codeblock
        #:lbl (label-text "Expanded code")
        (newline-split
#<<eos
(define-values [pos->vals init val-cont? ....]
  (make-sequence '(z)
                 (open-input-string "boris")))
(define-values [for-loop]
  (lambda (result pos)
    (let-values (([z] (pos->vals pos)))
      ....
      (if (if val-cont? (val-cont? z) #t)
        (+ result (add1 z))
        result))))
(for-loop 0 init)
eos
)))
    (define type
      (typed-codeblock
        #:lbl (label-text "Type environment")
        (newline-split
#<<eos
make-sequence
: (All (T)
   (-> Any (Sequenceof T)
       (Values
         (-> Any T)     ; pos->vals
         Any            ; init
         (U #f
            (-> T Any)) ; val-cont?
         ....)))
eos
)))
    (ht-append code-x-sep (vl-append code-y-sep src tgt) type)))

(define (fig:all-blame)
  (render-full-blame-table (get-full-blame-table benchmark-name*)))

(define (render-full-blame-table bt)
  (centered
   (tabular
    #:sep (hspace 3)
    #:style 'block
    #:row-properties '(bottom-border 1)
    #:column-properties '(left right)
    (list* (list "Benchmark" #;"Shallow" #;"Deep"
                 "Blame 0" "WH" "WH V" "WH V WB" "WH V WBS")
           (map cdr bt)))))

(define (get-full-blame-table name*)
  (for/list ([name (in-list name*)])
    (make-full-blame-row name
                         (benchmark-name->blame-info name "blame")
                         (benchmark-name->blame-info name "blame-weakhash")
                         (benchmark-name->blame-info name "blame-weakhash-value")
                         (benchmark-name->blame-info name "blame-weakhash-weakbox0")
                         (benchmark-name->blame-info name "blame-weakhash-weakbox1")
                         (benchmark-name->performance-info name stransient)
                         (benchmark-name->performance-info name transient-rkt-version))))

(define (make-full-blame-row name bd0 bd-wh bd-wh-v bd-wh-wb bd-wh-wbs pi-shallow pi-deep)
  (define t-baseline (performance-info->untyped-runtime pi-shallow))
  (list name
        (bm name)
        #;(rnd (max-overhead pi-shallow))
        #;(rnd (max-overhead pi-deep))
        (format-blame-data bd0 t-baseline)
        (format-blame-data bd-wh t-baseline)
        (format-blame-data bd-wh-v t-baseline)
        (format-blame-data bd-wh-wb t-baseline)
        (format-blame-data bd-wh-wbs t-baseline)))

(define-values [fig:model-interaction-x fig:model-interaction-y]
  (let* ((D (add-hubs (te-mode-text "Deep") 'D))
         (S (add-hubs (te-mode-text "Shallow") 'S))
         (U (add-hubs (te-mode-text "Untyped") 'U)))
    (values
      (ppict-do
        (blank 360 100)
        #:go (coord 0 10/100 'lt #:abs-x 10) D
        #:go (coord 1 10/100 'rt #:abs-x -10) S
        #:go (coord 1/2 1 'cb) U
        #:set
        (let* ((pp ppict-do-state)
               (lbl+arr*
                 (list
                   (list "wrap" 8 -9 (code-arrow 'D-E rt-find 'S-W lt-find (* 2/100 turn) (* 98/100 turn)  1/4 1/4 'solid))
                   (list "wrap" 8  19 (code-arrow 'S-W lb-find 'D-E rb-find (* 52/100 turn) (* 48/100 turn)  1/4 1/4 'solid))
                   ;;
                   (list "wrap" 8 9 (code-arrow 'D-S rb-find 'U-W lt-find (* 75/100 turn) (* 97/100 turn)  40/100 40/100 'solid))
                   (list "wrap" -65 30 (code-arrow 'U-W lb-find 'D-S lb-find (* 53/100 turn) (* 22/100 turn)  60/100 36/100 'solid))
                   ;;
                   (list "noop" -8 9 (code-arrow 'S-S lb-find 'U-E rt-find (* 75/100 turn) (* 53/100 turn)  40/100 40/100 'solid))
                   (list "scan" 59 30 (code-arrow 'U-E rb-find 'S-S rb-find (* 97/100 turn) (* 28/100 turn)  60/100 36/100 'solid))
                   )))
          (for/fold ((pp pp))
                    ((l+a (in-list lbl+arr*)))
            (add-code-arrow pp (fourth l+a) #:line-width 2 #:label (sc-text (first l+a) #:size (+ 1 title-text-size)) #:x-adjust-label (second l+a) #:y-adjust-label (third l+a)))))
      (ppict-do
        (blank 280 60)
        #:go (coord 0 25/100 'lt #:abs-x 10) S
        #:go (coord 1/2 0/100 'ct) D
        #:go (coord 1 25/100 'rt #:abs-x -10) U))))

(define fig:opt0
  ;; only D <-> S, weaken
  (ppict-do
    (blank 300 80)
    #:go (coord 0 30/100 'lt #:abs-x 10)
    (add-hubs (te-mode-text "Deep") 'D)
    #:go (coord 1 30/100 'rt #:abs-x -10)
    (add-hubs (te-mode-text "Shallow") 'S)
    #:set
    (let* ((pp ppict-do-state)
           (lbl+arr*
             (list
               (list "wrap, if value escapes to U" -10 -24 (code-arrow 'D-E rt-find 'S-W lt-find (* 11/100 turn) (* 89/100 turn)  1/4 1/4 'solid))
               (list "wrap, if value from U" 10  38 (code-arrow 'S-W lb-find 'D-E rb-find (* 61/100 turn) (* 39/100 turn)  1/4 1/4 'solid))
               )))
      (for/fold ((pp pp))
                ((l+a (in-list lbl+arr*)))
        (add-code-arrow pp (fourth l+a) #:line-width 2 #:label (sc-text (first l+a)) #:x-adjust-label (second l+a) #:y-adjust-label (third l+a))))))

(define fig:opt1
  (ppict-do
    (blank 400 80)
    #:go (coord 0 30/100 'lt #:abs-x 10)
    (add-hubs (te-mode-text "Deep") 'D)
    #:go (coord 45/100 30/100 'rt #:abs-x -10)
    (add-hubs (te-mode-text "Shallow") 'S)
    #:go (coord 1 30/100 'rt #:abs-x -10)
    (add-hubs (te-mode-text "Untyped") 'U)
    #:set
    (let* ((pp ppict-do-state)
           (pp (pin-arrows-line
                 6 pp
                 (find-tag pp 'D-E) rc-find (find-tag pp 'S-W) lc-find
                 #:label (sc-text "noop") #:line-width 2 #:style 'solid #:color "black"
                 #:x-adjust-label 0
                 #:y-adjust-label -4
                 #:start-angle 0 #:end-angle 0 #:start-pull 0 #:end-pull 0))
           (lbl+arr*
             (list
               (list "wrap, if value from D" -10 -24 (code-arrow 'S-E rt-find 'U-W lt-find (* 11/100 turn) (* 89/100 turn)  1/4 1/4 'solid))
               (list "wrap, if value escapes to D" 24  38 (code-arrow 'U-W lb-find 'S-E rb-find (* 61/100 turn) (* 39/100 turn)  1/4 1/4 'solid))
               )))
      (for/fold ((pp pp))
                ((l+a (in-list lbl+arr*)))
        (add-code-arrow pp (fourth l+a) #:line-width 2 #:label (sc-text (first l+a)) #:x-adjust-label (second l+a) #:y-adjust-label (third l+a))))))

(define fig:any-wrap
  (make-example-atom-pict
    '("(define b : (Boxof Symbol)"
      "  (box '$))"
      ""
      "(define any : Any b)")
    '("(set-box! any 'qq)")
    "(void)"
    "cannot write to box"))

(define fig:no-wrap
  (make-example-atom-pict
    '("(: add-mpair (-> (MPairof Real Real) Real))"
      "(define (add-mpair mp)"
      "  (+ (mcar mp) (mcdr mp)))")
    '("(add-mpair (mcons 2 4))")
    "6"
    "no contract for type"))

(define fig:index-of
  (let* ((uu
          (untyped-codeblock '(
            "(index-of '(a b) 'a)")))
         (tt
          (typed-codeblock '(
            "(require/typed racket/list"
            "  [index-of"
            "   (All (T)"
            "    (-> (Listof T) T"
            "        (U #f Natural)))])"
            ""
            "(index-of '(a b) 'a)")))
         (uu (vc-append caption-shim uu (code-text "Untyped: 0")))
         (tt (vc-append caption-shim
                        tt
                        (vc-append (* 1/2 caption-shim)
                          (code-text "Shallow: 0")
                          (code-text "Deep: #f")))))
    (ht-append (* 8 hshim) uu tt)))

(define-values [fig:ds-example-x fig:ds-example-y]
  (let* ((untyped
          ;; images/icons/symbol = 300 lines
          ;; images/icons/style = 200 lines
          ;; images/private = 3470 lines (includes flomap, raytrace code)
          (untyped-codeblock '(
            "(define (text s f)"
            "  ; render string s as"
            "  ; text with font f"
            "  ....)")))
         (interface
           (typed-codeblock '(
            "(require/typed/provide"
            " (text"
            "  (-> String Font"
            "      Bitmap)))")))
         (main
           (untyped-codeblock '(
            "(text \"cat\" \"roman\")"))))
    (values
      (ht-append hshim untyped right-arrow interface right-arrow main)
      (vc-append vshim untyped down-arrow interface down-arrow main))))

;; -----------------------------------------------------------------------------

(module+ raco-pict
  (provide raco-pict)
  (define raco-pict
    (add-rectangle-background #:color "white" #:x-margin 40 #:y-margin 40
      (apply vl-append 10
        ;; fig:model-interaction
        fig:any-wrap
        '()
    )))
)

