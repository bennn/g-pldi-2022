#lang at-exp slideshow

;; outline 2022-04-27
;; - ben postdoc at brown
;;
;; todo 2022-05-04
;; - [ ] all languages ... rough landscape
;;   - [X] do the typing labor
;; - [X] 4 directions, prior work
;; - [ ] interactions roadmap: W S N
;; - [X] performance ... tables and/or plots
;; - [ ] expressiveness slides
;; - [ ] conclusion ... should be easy no? from job slides
;; - [X] no wrappers = simpler top type
;; - [ ] uu recruiting slide

;; LATER
;; - [ ] utah fonts
;; - [ ] fix up colors
;; - [ ] ....

(require
  images/icons/misc
  images/icons/symbol
  images/icons/style
  images/icons/file
  images/icons/control
  (only-in pict/face face)
  (only-in math/statistics mean)
  (only-in racket/random random-sample)
  ;; "../img.rkt" ... copied from TransientExperience
  file/glob
  racket/class
  racket/draw
  racket/format
  racket/match
  racket/list
  racket/string
  racket/runtime-path
  gtp-pict
  pict
  ppict/2
  pict-abbrevs
  ppict/pict ppict/tag
  pict-abbrevs/slideshow
  (only-in slideshow para bt)
  gtp-plot/configuration-info gtp-plot/plot gtp-plot/typed-racket-info gtp-plot/reticulated-info gtp-plot/performance-info gtp-plot/sample-info
  plot/no-gui (except-in plot/utils min* max*))

[*OVERHEAD-DECORATION?* #f]

(define slide-top 4/100)
(define slide-left 2/100)
(define slide-right (- 1 slide-left))
(define slide-bottom 82/100)
(define slide-text-left (* 3 slide-left)) ;; 3/2 SD 4:3
(define head-left 20/100) ;; slide-left SD 4:3
(define head-right (- 1 head-left)) ;; slide-right SD 4:3
(define text-left slide-text-left)
(define slide-text-right (- 1 slide-text-left))
(define text-right slide-text-right)
(define slide-heading-top (* 1.4 slide-top))
(define slide-text-top (* 4 slide-top))
(define hi-text (* 6 slide-top))
(define lo-text (* 2.5 hi-text))
(define slide-text-bottom slide-bottom)

(define slide-text-coord (coord slide-text-left slide-text-top 'lt))
(define slide-text-coord-left slide-text-coord)
(define slide-text-coord-mid (coord 1/2 slide-text-top 'ct))
(define slide-text-coord-right (coord slide-text-right slide-text-top 'rt))
(define slide-text-coord-l  slide-text-coord-left)
(define slide-text-coord-m   slide-text-coord-mid)
(define slide-text-coord-r slide-text-coord-right)
(define heading-text-coord (coord head-left slide-heading-top 'lt))
(define heading-text-coord-left heading-text-coord)
(define heading-text-coord-mid (coord 1/2 slide-heading-top 'ct))
(define heading-text-coord-right (coord head-right slide-heading-top 'rt))
(define heading-coord heading-text-coord)
(define heading-coord-left heading-text-coord-left)
(define heading-coord-mid heading-text-coord-mid)
(define heading-coord-right heading-text-coord-right)
(define heading-coord-l  heading-coord-left)
(define heading-coord-m  heading-coord-mid)
(define heading-coord-r  heading-coord-right)
(define bottom-coord-left (coord slide-left slide-text-bottom 'lb))
(define bottom-coord-mid (coord 1/2 slide-text-bottom 'cb))
(define bottom-coord-right (coord slide-right slide-text-bottom 'rb))
(define center-coord (coord 1/2 1/2 'cc))
(define title-coord (coord 1/2 26/100 'ct))
(define hi-text-coord-left (coord slide-text-left hi-text 'lt))
(define hi-text-coord-mid (coord 1/2 hi-text 'ct))
(define hi-text-coord-right (coord slide-text-right hi-text 'rt))
(define hi-text-coord-l  hi-text-coord-left)
(define hi-text-coord-m   hi-text-coord-mid)
(define hi-text-coord-r hi-text-coord-right)
(define lo-text-coord-left (coord slide-text-left lo-text 'lt))
(define lo-text-coord-mid (coord 1/2 lo-text 'ct))
(define lo-text-coord-right (coord slide-text-right lo-text 'rt))
(define all-lang-coord (coord 99/100 1/2 'rc))

(define img "img")
(define src img)

(define default-line-width 4)
(define default-arrow-size 14)
(define large-arrow-size 18)

(define turn revolution)

(define x%->pixels w%->pixels)
(define y%->pixels h%->pixels)

(define pico-x-sep (w%->pixels 1/100))
(define tiny-x-sep (w%->pixels 2/100))
(define border-x-sep (w%->pixels 4/100))
(define small-x-sep (w%->pixels 5/100))
(define med-x-sep (w%->pixels 10/100))
(define big-x-sep (w%->pixels 15/100))

(define pico-y-sep (h%->pixels 1/100))
(define tiny-y-sep (h%->pixels 2/100))
(define small-y-sep (h%->pixels 5/100))
(define med-y-sep (h%->pixels 10/100))
(define big-y-sep (h%->pixels 15/100))

(define code-brush-alpha 0.6)

(define (color%++ c n)
  (make-object color%
               (byte-round (+ (send c red) n))
               (byte-round (+ (send c green) n))
               (byte-round (+ (send c blue) n))
               (send c alpha)))

(define (byte-round n)
  (if (< n 0)
    0
    (if (< 255 n)
      255 n)))


(define black (hex-triplet->color% #x222222))
(define onux-black (hex-triplet->color% #x121214))
(define gray (string->color% "light gray"))
(define white (string->color% "white"))
(define lite-grey (hex-triplet->color% #xeeeeee)) ; "gainsboro"
(define transparent (color%-update-alpha white 0))
(define dark-orange (hex-triplet->color% #xE05626))
(define lite-orange (hex-triplet->color% #xF89C3F))
(define dark-blue (hex-triplet->color% #x002E6D))
(define bg-dark-blue (hex-triplet->color% #x2C6B91))
(define bg-lite-blue (hex-triplet->color% #x357C9F))
(define lite-blue (hex-triplet->color% #xC0EFFF))
(define lite-green (hex-triplet->color% #x00b18f))
(define typed-color lite-orange)
(define untyped-color lite-green)
(define shallow-color dark-orange)
(define deep-color typed-color)
(define typed-brush-color (color%++ typed-color 20))
(define shallow-pen-color shallow-color #;(hex-triplet->color% #xffc20a) )
(define deep-pen-color (hex-triplet->color% #x0c7bdc))
(define shallow-brush-color lite-orange #;(hex-triplet->color% #xfdc008))
(define deep-brush-color deep-pen-color #;(hex-triplet->color% #x0a79da))
(define untyped-brush-color (color%++ untyped-color 20))
(define fog-3k1 (hex-triplet->color% #xDBCAC2))
(define neutral-brush-color fog-3k1)
(define green0-3k1 (hex-triplet->color% #x71BE8D))
(define green1-3k1 (hex-triplet->color% #x598F61))
(define green2-3k1 (hex-triplet->color% #x4F7459))
(define red0-3k1 (hex-triplet->color% #xF0749C))
(define red1-3k1 (hex-triplet->color% #xC3476F))
(define apple-green lite-green)
(define apple-red red1-3k1)
(define typed-pen-color #f)
(define untyped-pen-color #f)
(define validate-pen-color red1-3k1)
(define validate-brush-color (color%-update-alpha validate-pen-color code-brush-alpha))
(define happy-cloud-color lite-blue)
(define sad-cloud-color dark-blue)
(define default-line-color bg-lite-blue)
(define browncs-frame-color dark-blue)
(define hilite-frame-color dark-orange)
(define triangle-blue (color%++ bg-lite-blue 30))
(define alloy-brush-color lite-blue)
(define wong-red (hex-triplet->color% #xd55e00))
(define wong-green (hex-triplet->color% #x009e73))
(define traffic-green-hi (string->color% "light green"))
(define traffic-yellow-hi (string->color% "yellow"))
(define traffic-red-hi (string->color% "firebrick"))
(define blame-color typed-color)

(define (color-off c)
  (color%-update-alpha c 0.2))

(define title-font "Bree Serif")
(define code-font "Inconsolata")
(define body-font "Open Sans")

(define title-size 52)
(define subtitle-size 34)
(define head-size 40)
(define body-size 30)
(define code-size 28)
(define tcode-size (- code-size 4))

(define ((make-string->text #:font font #:size size #:color color) . str*)
  (colorize (text (apply string-append str*) font size) color))

(define (bold-style font)
  (cons 'bold font))

(define (italic-style font)
  (cons 'italic font))

(define body-font-lo (make-object font% body-size body-font 'default 'normal 'light))
(define body-font-it (make-object font% body-size body-font 'default 'italic 'light))
(define body-font-itbf (make-object font% body-size body-font 'default 'italic 'semibold))
(define body-font-md (make-object font% body-size body-font 'default 'normal 'medium))
(define body-font-hi (make-object font% body-size body-font 'default 'normal 'semibold))
(define page-font (make-font #:face code-font #:size tcode-size))

(define titlerm (make-string->text #:font title-font #:size title-size #:color black))
(define subtitlerm (make-string->text #:font title-font #;body-font-md #:size subtitle-size #:color black))
(define subtitlermlo
  (let ((ff (make-string->text #:font title-font #:size subtitle-size #:color black)))
    (lambda str*
      (cellophane (apply ff str*) 0.7))))
(define headrm (make-string->text #:font title-font #:size head-size #:color dark-blue))
(define coderm (make-string->text #:font code-font #:size code-size #:color black))
(define codebf (make-string->text #:font (bold-style code-font) #:size code-size #:color black))
(define codeemrm (make-string->text #:font (bold-style code-font) #:size code-size #:color green2-3k1))
(define codeemrm2 (make-string->text #:font (bold-style code-font) #:size code-size #:color dark-orange))
(define codeembf (make-string->text #:font (bold-style code-font) #:size code-size #:color apple-red))
(define tcoderm (make-string->text #:font code-font #:size tcode-size #:color black))
(define tcodebf (make-string->text #:font (bold-style code-font) #:size tcode-size #:color black))
(define tt coderm)

(define bodyrm (make-string->text #:font body-font-md #:size body-size #:color black))
(define bodyrmlo (make-string->text #:font body-font-lo #:size body-size #:color black))
(define bodyrmlobb (make-string->text #:font body-font-lo #:size body-size #:color deep-pen-color))
(define bodyrmloyy (make-string->text #:font body-font-lo #:size body-size #:color shallow-pen-color))
(define bodyrmhi (make-string->text #:font body-font-hi #:size body-size #:color black))
(define bodyrmhibb (make-string->text #:font body-font-hi #:size body-size #:color deep-pen-color))
(define bodyrmhiyy (make-string->text #:font body-font-hi #:size body-size #:color shallow-pen-color))
(define bodyit (make-string->text #:font body-font-it #:size body-size #:color black))
(define bodyitbf (make-string->text #:font body-font-itbf #:size body-size #:color black))
(define bodybf (make-string->text #:font (bold-style body-font) #:size body-size #:color black))
(define bodyemit (make-string->text #:font body-font-it #:size body-size #:color dark-orange))
(define bodyemrm (make-string->text #:font body-font-md #:size body-size #:color dark-orange))
(define bodyembf (make-string->text #:font (bold-style body-font) #:size body-size #:color dark-orange))
(define bodyemrm2 (make-string->text #:font body-font-md #:size body-size #:color green2-3k1))
(define bodyembf2 (make-string->text #:font (bold-style body-font-md) #:size body-size #:color green2-3k1))
(define bodyembf3 (make-string->text #:font (bold-style body-font-md) #:size body-size #:color apple-red))
(define bodyemty (make-string->text #:font body-font-md #:size body-size #:color deep-pen-color))
(define bodyemun (make-string->text #:font body-font-md #:size body-size #:color untyped-color))
(define bodyembl (make-string->text #:font body-font-md #:size body-size #:color blame-color))

(define bname bodyembf)

(define stransient "transient")
(define MAX-OVERHEAD 20)
(define gtp-version "6.0")
(define olde-rkt-version "6.2")
(define transient-rkt-version "7.8.0.5")
(define SAMPLE-RATE 10)
(define NUM-SAMPLE-TRIALS 10)

(define (performance-info->sample-info pi #:replacement? [replace? #f])
  (define num-in-sample
    (* SAMPLE-RATE (performance-info->num-units pi)))
  (define rand-gen
    (vector->pseudo-random-generator '#(2022 01 26 19 09 00)))
  (define sample*
    (for/list ((_i (in-range NUM-SAMPLE-TRIALS)))
      (random-sample (in-configurations pi) num-in-sample rand-gen #:replacement? replace?)))
  (make-sample-info pi sample*))

(define-runtime-path here ".")
(define data-dir (build-path here ".." "data"))

(define (benchmark-name->data-file bm-name version)
  (define pp
    (let* ((name bm-name)
           (patt (format "~a-*rktd" name)))
      (glob-first (build-path data-dir version patt))))
  (if (file-exists? pp)
      pp
      (raise-argument-error 'benchmark-name->data-file "directory-exists?" pp)))


(define (benchmark-name->performance-info bm-name version #:full-name? [full-name? #f])
  (when (and (eq? bm-name 'zordoz)
             (string=? version "6.4"))
    (error 'die))
  (define data-dir (benchmark-name->data-file bm-name version))
  (define extra-name (and full-name? (string->symbol (format "~a-~a" bm-name version))))
  (make-typed-racket-info data-dir #:name extra-name))

(define (glob-first str)
  (match (glob str)
   [(cons r '())
    r]
   ['()
    (raise-user-error 'glob-first "No results for glob '~a'" str)]
   [r*
    (printf "WARNING: ambiguous results for glob '~a'. Returning the first.~n" str)
    (car r*)]))

(define make-converter
  (let ((magic-n (*OVERHEAD-LINE-COLOR*)))
    (lambda (a b)
      (lambda (i)
        (if (= i magic-n) a b)))))

(define pen-color-converter (make-converter deep-pen-color shallow-pen-color))
(define brush-color-converter (make-converter deep-brush-color shallow-brush-color))

(define (arrowhead-pict rad #:color [color black] #:size [size 20])
  (colorize
    (arrowhead 20 rad)
    color))

(define up-arrow-pict
  (arrowhead-pict (* 1/4 turn) #:color black))

(define right-arrow-pict
  (arrowhead-pict (* 0 turn) #:color black))

(define down-arrow-pict
  (arrowhead-pict (* 3/4 turn) #:color black))

(define make-bg
  (let ((*cache (box #f)))
    (lambda (w h)
      (or (unbox *cache)
          (let* ((bg (rotate (bitmap "img/browncs-bg.png") (* 3/4 turn)))
                 (bg (clip-to (scale-to-fit bg w h #:mode 'distort) w h))
                 (fg (filled-rectangle w (* 9/10 h) #:color white #:draw-border? #f))
                 (pp (freeze (cc-superimpose bg fg))))
            (set-box! *cache pp)
            pp)))))

(define (make-titlebg w h)
  (let* ((bg (rotate (bitmap "img/browncs-bg.png") (* 3/4 turn)))
         (bg (clip-to (scale-to-fit bg w h #:mode 'distort) w h))
         #;(fg (filled-rectangle w (* 9/10 h) #:color white #:draw-border? #f)))
    bg))

(define browncs-x-margin (make-parameter small-x-sep))

(define (browncs-box pp #:color [color white] #:x-margin [x-margin #f] #:y-margin [y-margin #f] #:frame-color [frame-color #f])
  (add-rounded-border
    pp
    #:x-margin (or x-margin (browncs-x-margin))
    #:y-margin (or y-margin tiny-y-sep)
    #:radius 2
    #:background-color color
    #:frame-width 2
    #:frame-color (or frame-color browncs-frame-color)))

(define (hilite-box pp #:color [color white] #:x-margin [x-margin #f] #:y-margin [y-margin #f] #:frame-color [frame-color #f])
  (add-rounded-border
    pp
    #:x-margin (or x-margin (browncs-x-margin))
    #:y-margin (or y-margin tiny-y-sep)
    #:radius 2
    #:background-color color
    #:frame-width 4
    #:frame-color (or frame-color hilite-frame-color)))

(define (browncs-frame pp)
  (browncs-box pp #:x-margin 0 #:y-margin 0))

(define (cite-box pp)
  (browncs-box #:color white pp))

(struct code-arrow (src-tag src-find tgt-tag tgt-find start-angle end-angle start-pull end-pull style) #:transparent)

(define (add-code-arrow pp arrow
                        #:both [both-arrow #f]
                        #:arrow-size [pre-arrow-size #f]
                        #:line-width [pre-line-width #f]
                        #:color [color #f]
                        #:label [label (blank)]
                        #:x-adjust-label [x-label 0]
                        #:y-adjust-label [y-label 0]
                        #:hide? [hide? #false])
  (define line-width (or pre-line-width default-line-width))
  (define arrow-size (or pre-arrow-size default-arrow-size))
  ((if both-arrow pin-arrows-line pin-arrow-line)
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
    #:color (or color default-line-color)))

(define (add-code-line pp arrow
                       #:line-width [pre-line-width #f]
                       #:color [color default-line-color]
                       #:label [label (blank)]
                       #:x-adjust-label [x-label 0]
                       #:y-adjust-label [y-label 0]
                       #:hide? [hide? #false])
  (add-code-arrow pp arrow #:arrow-size 0
                  #:line-width pre-line-width #:color color #:label label
                  #:x-adjust-label x-label #:y-adjust-label y-label #:hide? hide?))

(define (add-code-arrows pp #:arrow-size [arrow-size #f] #:color [color #f] . arrow*)
  (add-code-arrows* pp arrow* #:arrow-size arrow-size #:color color))

(define (add-code-arrows* pp* arrow* #:color [color #f] #:arrow-size [arrow-size #f])
  (for/fold ((pp pp*))
            ((arrow (in-list arrow*)))
    (add-code-arrow pp arrow #:color color #:arrow-size arrow-size)))

(define add-code-arrow* add-code-arrows*)

(define (add-code-lines pp #:color [color #f] . arrow*)
  (add-code-line* pp arrow* #:color color))

(define (add-code-line* pp arrow* #:color [color #f])
  (for/fold ((pp pp))
            ((arrow (in-list arrow*)))
    (add-code-line pp arrow #:color color)))

(define (ben-rule w h #:color [color #f])
  (filled-rectangle w h #:color (or color browncs-frame-color) #:draw-border? #f))

(define (vrule h #:thickness [thickness #f] #:color [color #f])
  (ben-rule (or thickness 1) h #:color color))

(define (hrule w #:thickness [thickness #f] #:color [color #f])
  (ben-rule w (or thickness 1) #:color color))

(define (double-down-arrow h)
  (define vr (vrule h #:color black))
  (ppict-do
    (ht-append 3 vr vr)
    #:go (coord 1/2 1 'cc)
    (arrowhead-pict (* 3/4 turn) #:size default-arrow-size )))

(define (thick-right-arrow w)
  (ppict-do
    (filled-rectangle w 8 #:color black #:draw-border? #f)
    #:go (coord 1 1/2 'cc)
    (arrowhead (* 2 default-arrow-size) 0)))

(define (thick-down-arrow h)
  (ppict-do
    (vrule h #:thickness 8 #:color black)
    #:go (coord 1/2 1 'cc)
    (arrowhead (* 2 default-arrow-size) (* 3/4 turn))))

(define (scale-to-pict pp bg)
  (scale-to-fit pp (pict-width bg) (pict-height bg)))

(define (scale-to-huge pp)
  (scale-to-fit pp
    (w%->pixels 9/10)
    (h%->pixels 85/100)))

(define (scale-to-superscript pp)
  (scale-to-fit pp
    44
    44))

(define (scale-to-tiny pp)
  (scale-to-fit pp 50 50))

(define (scale-to-width pp w)
  (scale-to-fit pp w (pict-height pp)))

(define (scale-to-width% pp w%)
  (scale-to-width pp (w%->pixels w%)))

(define (scale-to-height% pp h%)
  (scale-to-height pp (h%->pixels h%)))

(define (scale-to-height pp h)
  (scale-to-fit pp (pict-width pp) h))

(define (scale-to-square pp dim)
  (scale-to-fit pp dim dim))

(define (person-scale pp [w% #f])
  (scale-to-width% pp (or w% 15/100)))

(define (person-frame pp)
  (add-rounded-border
    pp
    #:radius 2
    #:frame-width 1
    #:frame-color black))

(define (typed-person-frame pp)
  (X-person-frame pp typed-color))

(define (untyped-person-frame pp)
  (X-person-frame pp untyped-color))

(define (X-person-frame pp cc)
    (add-rounded-border
      (person-frame pp)
      #:x-margin tiny-y-sep
      #:y-margin tiny-y-sep
      #:radius 2
      #:frame-width 0
      #:background-color cc))

(define (add-lang str)
  (string-append "lang/" str))

(define (scale-small-face pp)
  (scale-to-square pp 80))

(define (add-face str)
  (string-append "face/" str))

(define (add-src str)
  (string-append "img/" str))

(define add-img add-src)

(define (frame-person _f str w)
  (person-frame
    (person-scale (bitmap (add-src (add-face str))) w)))

(define (person-pict str)
  (person-frame (person-scale (bitmap str))))

(define (typed-person-pict str)
  (typed-person-frame (person-scale (bitmap str) 10/100)))

(define (untyped-person-pict str)
  (untyped-person-frame (person-scale (bitmap str) 10/100)))

(define word-sep 0)

(define (word-append . pp*)
  (apply hb-append word-sep pp*))

(define line-sep2 (- 2))

(define (left-line-append2 . pp*)
  (left-line-append2* pp*))

(define (left-line-append2* pp*)
  (apply vl-append line-sep2 pp*))

(define (mid-line-append2 . pp*)
  (mid-line-append2* pp*))

(define (mid-line-append2* pp*)
  (apply vc-append line-sep2 pp*))

(define (right-line-append2 . pp*)
  (right-line-append2* pp*))

(define (right-line-append2* pp*)
  (apply vr-append line-sep2 pp*))

(define ll-append left-line-append2)
(define lc-append mid-line-append2)
(define lr-append right-line-append2)

(define line-sep tiny-y-sep)

(define (left-line-append #:sep [sep #f] . pp*)
  (left-line-append* #:sep sep pp*))

(define l-line-append left-line-append)

(define (left-line-append* #:sep [sep #f] pp*)
  (apply vl-append (or sep line-sep) pp*))

(define (mid-line-append #:sep [sep #f] . pp*)
  (apply vc-append (or sep line-sep) pp*))

(define m-line-append mid-line-append)

(define (right-line-append . pp*)
  (apply vr-append line-sep pp*))

(define r-line-append right-line-append)

(define code-line-sep (h%->pixels 12/1000))

(define (code-line-append . pp*)
  (code-line-append* pp*))

(define (code-line-append* pp*)
  (apply vl-append code-line-sep pp*))

(define (codeblock-append #:sep [sep #f] . pp*)
  (codeblock-append* pp*))

(define (codeblock-append* #:sep [sep #f] pp*)
  (apply vl-append (or sep tiny-y-sep) pp*))

(define (hcodeblock-append #:sep [sep #f] . pp*)
  (hcodeblock-append* #:sep sep pp*))

(define (hcodeblock-append* #:sep [sep #f] pp*)
  (apply ht-append (or sep tiny-x-sep) pp*))

(define boundary-append* hcodeblock-append*)

(define (scale-lang-lo pp)
  (scale-to-fit pp 120 80))

(define (scale-lang-hi pp)
  (scale-to-fit pp 120 120))

(define (lang-lo str)
  (scale-lang-lo (bitmap str)))

(define (symbol->lang-pict sym #:ext [ext #f])
  (lang-lo (add-img (add-lang (format "~a.~a" sym (or ext 'png))))))

(define (symbol->lang-pict2 sym)
  (label-below
    (add-hubs (symbol->lang-pict sym) sym)
    (bodyrmlo
      (if (eq? sym 'php) "PHP" (string-titlecase (symbol->string sym))))))

(define (lang-hi str)
  (scale-lang-hi (bitmap str)))

(define (frame-team title-pict . name*)
  (frame-team* title-pict name*))

(define (frame-team* title-pict name*)
  (define pp* (for/list ((n (in-list name*))) (frame-person #f n 9/100)))
  (define sep 2)
  (define author-grid
    (let loop ((pp* pp*))
      (cond
        [(null? pp*)
         (blank)]
        [(null? (cdr pp*))
         (car pp*)]
        [else
         (vc-append sep (ht-append sep (car pp*) (cadr pp*)) (loop (cddr pp*)))])))
  #;(vc-append pico-y-sep title-pict author-grid)
  author-grid)

(define (split/2 lang-img*)
  (split-at lang-img* (quotient (length lang-img*) 2)))

(define (split/n lang-img* n)
  (let loop ((pp* lang-img*))
    (if (< (length pp*) n)
      (list pp*)
      (let-values (((a b) (split-at pp* n)))
        (cons a (loop b))))))

(define (v-split-append lang-img* #:x [xsep #f])
  (define-values [a* b*] (split/2 lang-img*))
  (hc-append
    (or xsep tiny-x-sep)
    (apply vc-append tiny-y-sep a*)
    (apply vc-append tiny-y-sep b*)))

(define (X-codeblock pp* #:dark? [dark? #f] #:title [title #f] #:label [label #f] #:frame-color [frame-color #f] #:background-color [background-color #f])
  (define title-pict (if (pict? title) title (if (string? title) (bodyrmlo title) #f)))
  (define label-margin (if title-pict (* 10/100 (pict-height title-pict)) 0))
  (define (add-label-margin pp [extra 0]) (vl-append (+ extra label-margin) (blank) pp))
  (define radius 1)
  (define fw 5)
  (let* ((block-pict
           (browncs-box
             (code-line-append* pp*)
             #:frame-color #f #;(if dark? #f background-color)
             #:color (if dark?
                       background-color
                       (color%-update-alpha background-color 0.4))
                       )))
    (if label
      (let ((block-pict (add-label-margin block-pict 2)))
        (ppict-do (if title-pict (lt-superimpose block-pict (ht-append 4 (blank) title-pict)) block-pict)
          #:go (coord 1/2 0 'ct) label))
      (if title-pict (vl-append 0 (ht-append 4 (blank) title-pict) (add-label-margin block-pict)) block-pict))))

(define (conslang x y)
  (if x (list* (tt x) (blank) y) y))

(define (untyped-code str)
  (untyped-codeblock #:title #f #:lang #f str))

(define (untyped-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang untyped"] . str*)
  (untyped-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (untyped-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color untyped-pen-color #:background-color untyped-brush-color))

(define (shallow-code str)
  (shallow-codeblock #:title #f #:lang #f str))

(define (shallow-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang shallow"] . str*)
  (shallow-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (shallow-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color shallow-pen-color #:background-color shallow-brush-color))

(define (deep-code str)
  (deep-codeblock #:title #f #:lang #f str))

(define (deep-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang deep"] . str*)
  (deep-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (deep-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color deep-pen-color #:background-color deep-brush-color))

(define typed-codeblock* deep-codeblock*)

(define (untyped-box pp)
  (browncs-box #:x-margin 0 #:y-margin 0 #:color untyped-brush-color pp))

(define (typed-box pp)
  (browncs-box #:x-margin 0 #:y-margin 0 #:color deep-brush-color pp))

(define (typed-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang typed"] . str*)
  (deep-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (dyn-codeblock sym)
  (case sym
    ((U untyped) untyped-codeblock)
    ((D deep) deep-codeblock)
    ((S shallow) shallow-codeblock)
    ((T typed) typed-codeblock)
    ((#f) (lambda arg* (blank)))
    (else (raise-argument-error 'dyn-codeblock "(or/c D S U)" sym))))

(define (xblank n)
  (blank n 0))

(define (yblank n)
  (blank 0 n))

(define xsep xblank)
(define ysep yblank)

(define natural-str "Guarded")
(define conatural-str "Co-Guarded")
(define forgetful-str "Forgetful")
(define transient-str "Transient")
(define amnesic-str "Amnesic")
(define erasure-str "Optional")

(define (check-pict size)
  (define outer-color green1-3k1)
  (define inner-color green0-3k1)
  (define line-width% 6)
  ;;
  (define size/2 (/ size 2))
  (define size/3 (/ size 3))
  (define line-width (/ size line-width%))
  (define line-width/2 (/ line-width 2))
  (define F 50/100)
  ;;
  (define (draw-check dc% dx dy)
    (define old-brush (send dc% get-brush))
    (define old-pen (send dc% get-pen))
    ;;
    (send dc% set-brush (new brush% [color inner-color]))
    (send dc% set-pen (new pen% [width 1] [color outer-color]))
    ;; draw check from mid-left
    (define path% (new dc-path%))
    (send path% move-to (+ line-width/2 2) (* F size))
    (send path% line-to (- size/2 (/ line-width 2)) size)
    (send path% line-to (+ size/2 (/ line-width 4)) size)
    (send path% line-to size 0)
    (send path% line-to (- size line-width) 0)
    (send path% line-to (- size/2 (/ line-width 8)) (- size line-width))
    (send path% line-to (- size/2 (/ line-width 5)) (- size line-width))
    (send path% line-to (+ (* 1.6 line-width) 2) (* F size))
    (send path% close)
    (send dc% draw-path path% dx dy)
    ;;
    (send dc% set-brush old-brush)
    (send dc% set-pen old-pen)
    (void))
  (dc draw-check size size))

(define (x-pict size)
  (define outer-color red1-3k1)
  (define inner-color red0-3k1)
  (define line-width% 6)
  ;;
  (define size/2 (/ size 2))
  (define line-width (/ size line-width%))
  (define line-width/2 (/ line-width 2))
  ;;
  (define (draw-x dc% dx dy)
    (define old-brush (send dc% get-brush))
    (define old-pen (send dc% get-pen))
    ;;
    (send dc% set-brush (new brush% [color inner-color]))
    (send dc% set-pen (new pen% [width 1] [color outer-color]))
    ;; draw X from top-left, counterclockwise
    (define path% (new dc-path%))
    (send path% move-to 0 0)
    (send path% line-to (- size/2 line-width/2) size/2)
    (send path% line-to 0 size)
    (send path% line-to line-width size)
    (send path% line-to size/2 (+ size/2 line-width/2))
    (send path% line-to (- size line-width) size)
    (send path% line-to size size)
    (send path% line-to (+ size/2 line-width/2) size/2)
    (send path% line-to size 0)
    (send path% line-to (- size line-width) 0)
    (send path% line-to size/2 (- size/2 line-width/2))
    (send path% line-to line-width 0)
    (send path% close)
    (send dc% draw-path path% dx dy)
    ;;
    (send dc% set-brush old-brush)
    (send dc% set-pen old-pen)
    (void))
  (dc draw-x size size))

(define pass-pict
  (check-pict 40))

(define fail-pict
  (x-pict 40))

(define y-str pass-pict)
(define n-str fail-pict)

(define design-pre-bg-table*
  (list
    (list "" natural-str transient-str erasure-str)
    (list "type soundness" y-str y-str n-str)
    ;; Not static, because the model doesn't actually satisfy static
    ;;  and it's irrelevant for semantics
    (list "dyn. gradual guarantee" y-str y-str y-str)
    (list "blame theorem" y-str y-str y-str)))

(define design-m11-bg-table*
  (list
    (list "" natural-str transient-str)
    (list "type soundness" y-str y-str)
    (list "dyn. gradual guarantee" y-str y-str)
    (list "blame theorem" y-str y-str)))

(define design-m22-bg-table*
  (list
    (list ""                natural-str transient-str)
    (list "complete monitoring"   y-str n-str)
    (list "blame soundness"       y-str n-str)
    (list "blame completeness"    y-str n-str)
  ))

(define (bghost pp)
  (blank (pict-width pp) (pict-height pp)))

(define (add-neutral-background pp)
  (add-rectangle-background
    #:x-margin 0 #:y-margin 0
    #:color neutral-brush-color
    #:radius 1
    (add-rectangle-background
      #:x-margin small-x-sep #:y-margin tiny-y-sep
      #:color white
      pp)))

(define (table2 #:col-sep [pre-col-sep #f]
                #:row-sep [pre-row-sep #f]
                #:col-align [col-align lc-superimpose]
                #:row-align [row-align cc-superimpose]
                . kv*)
  (table2* kv*
                     #:col-sep pre-col-sep
                     #:row-sep pre-row-sep
                     #:col-align col-align
                     #:row-align row-align))

(define (table2* kv**
                     #:col-sep [pre-col-sep #f]
                     #:row-sep [pre-row-sep #f]
                     #:col-align [col-align lc-superimpose]
                     #:row-align [row-align cc-superimpose])
  (define col-sep (or pre-col-sep 328/5))
  (define row-sep (or pre-row-sep 364/5))
  (table 2 (flatten kv**) col-align row-align col-sep row-sep))

(define (xindent pp #:sep [x #f]) (ht-append (xblank (or x small-x-sep)) pp))

(define swatch-blank (blank pico-x-sep pico-y-sep))

(define red-swatch
  (X-codeblock #:background-color apple-red #:dark? #t #:title #f (list swatch-blank)))

(define untyped-swatch
  (untyped-codeblock* #:dark? #t #:title #f (list swatch-blank)))

(define shallow-swatch
  (shallow-codeblock* #:dark? #t #:title #f (list swatch-blank)))

(define deep-swatch
  (deep-codeblock* #:dark? #t #:title #f (list swatch-blank)))

(define big-swatch-blank (blank med-x-sep small-y-sep))

(define (untyped-icon #:lbl [lbl "U"])
  (center-label
    (untyped-codeblock* #:title #f (list big-swatch-blank))
    lbl))

(define (typed-icon #:lbl [lbl "T"])
  (center-label
    (deep-codeblock* #:title #f (list big-swatch-blank))
    lbl))

(define (untyped-icon2 pp)
  (untyped-codeblock* #:title #f (list (cc-superimpose big-swatch-blank pp))))

(define (typed-icon2 pp)
  (typed-codeblock* #:title #f (list (cc-superimpose big-swatch-blank pp))))

(define (center-label pp lbl)
  (ppict-do
    pp
    #:go (coord 1/2 46/100 'cc)
    (if lbl (scale (headrm lbl) 0.9) (blank))))

(define (dyn-swatch sym)
  (case sym
    ((D T) deep-swatch)
    ((U) untyped-swatch)
    ((S) shallow-swatch)
    ((B) (bghost deep-swatch))
    ((K) red-swatch)
    (else (raise-argument-error 'dyn-swatch "(or/c 'D 'S 'U)" sym))))

(define (boundary-node sym* #:arrow [arrow #f])
  (let* ((pp*
          (for/list ((sym (in-list sym*))
                     (i (in-naturals)))
            (add-hubs
              (dyn-swatch sym)
              (string->symbol (format "N~a" i)))))
         (pp (hcodeblock-append* #:sep (if arrow tiny-x-sep 4) pp*))
         (gg (bghost pp)))
    (if arrow
      (add-code-arrows
        ;; TODO options? gotta standardize everywhere
        (vc-append gg pp gg)
        (code-arrow 'N0-E rc-find 'N1-W lc-find 0 0 0 0 'solid)
        (code-arrow 'N2-W lc-find 'N1-E rc-find (* 1/2 turn) (* 1/2 turn) 0 0 'solid)
        (code-arrow 'N3-E rc-find 'N4-W lc-find 0 0 0 0 'solid)
        (code-arrow 'N0-N rt-find 'N3-N lt-find (* 08/100 turn) (* 92/100 turn) 1/4 1/4 'solid)
        (code-arrow 'N2-S rb-find 'N4-S lb-find (* 90/100 turn) (* 10/100 turn) 1/4 1/4 'solid))
      pp)))

(define the-boundary-pict
  (boundary-node '(U D U D D) #:arrow #t))

(define the-mixed-pict
  (boundary-node '(U D U D D)))

(define the-typed-pict
  (boundary-node '(D D D D D)))

(define the-untyped-pict
  (boundary-node '(U U U U U)))

(define lattice-x-sep small-x-sep)
(define lattice-y-sep tiny-y-sep)

(define (path-node sym*)
  (apply hc-append pico-x-sep (map dyn-swatch sym*)))

(define (bits->yes/no b*)
  (define node (bits->path-node b*))
  (define mark (if (eq?* b*) y-str n-str))
  (cc-superimpose (bghost node) (scale-to-height mark (pict-height node))))

(define (eq?* x*)
  (let loop ((xx x*))
    (if (or (null? xx) (null? (cdr xx)))
      #t
      (and (eq? (car xx) (cadr xx))
           (loop (cdr xx))))))

(define (bits->path-node b*)
  (path-node
    (for/list ((b (in-list b*)))
      (if b 'T 'U))))

(define (what-to-measure-lattice n #:x [x #f])
  (define tu-pict
    (let* ((u-pict (path-node (make-list n 'U)))
           (t-pict (path-node (make-list n 'T))))
      (vl-append (h%->pixels 35/1000) t-pict u-pict)))
  (define x-sep
    (if (< n 5) lattice-x-sep pico-x-sep))
  (define lattice-pict
    (make-lattice n (if x bits->yes/no bits->path-node) #:x-margin x-sep #:y-margin lattice-y-sep))
  (values tu-pict lattice-pict))

(define (scale-small-lattice pp)
  (scale-to-fit pp (w%->pixels 55/100) (h%->pixels 45/100)))

(define (d-deliv-icon w)
  (ppict-do
    (blank w w)
    #:go (coord 44/100 5/100 'lt) (dd-pict 5)
    #:go (coord 90/100 1/2 'rt) (clock-pict (* 40/100 w))
    #:go (coord 0 1/2 'lc) (face-pict (* 55/100 w))))

(define (clock-pict w)
  (bitmap (clock-icon #:height w)))

(define (lock-pict w [open? #f])
  (bitmap (lock-icon open? #:height w)))

(define (face-pict w)
  (scale-to-fit (face 'happy) w w))

(define (dd-pict d-num)
  (cloud-background2
    (word-append (bodybf (format "~a" d-num)) @bodyrmhi{x})))

(define (cloud-background2 pp)
  (cc-superimpose
    (cloud (* 2 (pict-width pp))
           (* 1.4 (pict-height pp))
           gray
           #:style '(square))
    pp))

(define (migration-append #:arr [arr #f] . pp*)
  (migration-append* #:arr arr pp*))

(define (migration-append* #:arr [arr #f] pp*)
  (apply vc-append lattice-y-sep (add-between pp* (or arr up-arrow-pict))))

(define (dmigration-append #:arr [arr #f] . pp*)
  (dmigration-append* #:arr arr pp*))

(define (dmigration-append* #:arr [arr #f] pp*)
  (apply vc-append lattice-y-sep (add-between pp* (or arr down-arrow-pict))))

(define (hmigration-append #:arr [arr #f] . pp*)
  (hmigration-append* #:arr arr pp*))

(define (hmigration-append* #:arr [arr #f] pp*)
  (apply hc-append pico-x-sep (add-between pp* (or arr right-arrow-pict))))

(define-syntax-rule (with-plot-params w h base-color exp)
  (parameterize ([*OVERHEAD-MAX* MAX-OVERHEAD]
                 [*OVERHEAD-LEGEND?* #false]
                 [*OVERHEAD-PLOT-WIDTH* w]
                 [*OVERHEAD-PLOT-HEIGHT* h]
                 [*OVERHEAD-LINE-WIDTH* 3]
                 #;[*OVERHEAD-LINE-COLOR* base-color]
                 [*PEN-COLOR-CONVERTER* pen-color-converter]
                 [*BRUSH-COLOR-CONVERTER* brush-color-converter]
                 [*INTERVAL-ALPHA* code-brush-alpha]
                 [*MULTI-INTERVAL-ALPHA* code-brush-alpha]
                 ;[x-axis-ticks? #f]
                 ;[y-axis-ticks? #f]
                 )
    exp))

(define ((deco->pi bm-name) d)
  (case d
    ((D)
     (benchmark-name->performance-info bm-name transient-rkt-version))
    ((D2)
     (list
       (benchmark-name->performance-info bm-name olde-rkt-version)
       (benchmark-name->performance-info bm-name transient-rkt-version)))
    ((best)
     (define pi-deep (benchmark-name->performance-info bm-name transient-rkt-version))
     (define pi-shallow (benchmark-name->performance-info bm-name stransient))
     (make-typed-racket-info
       (for/vector ((deep-cfg (in-configurations pi-deep))
                    (shallow-cfg (in-configurations pi-shallow)))
         (define deep-t* (configuration-info->runtime* deep-cfg))
         (define shallow-t* (configuration-info->runtime* shallow-cfg))
         (if (< (mean deep-t*) (mean shallow-t*))
           deep-t*
           shallow-t*))))
    (else (raise-argument-error 'big-overhead-plot "(or/c 'D 'S 'best)" d))))

(define (overhead-pict pre-pi* ds
                          #:wh [wh #f])
  (define-values [w h]
    (if wh
      (values (car wh) (cdr wh))
      (values (w%->pixels 32/100) (h%->pixels 15/100))))
  (define pi* (map (lambda (x) ((deco->pi x) ds)) pre-pi*))
  (define base-color
    (if (and (not (null? pi*))
             (null? (cdr pi*))
             (reticulated-info? (car pi*)))
      1
      0))
  (define pp
    (with-plot-params w h base-color
      (overhead-plot pi*)))
  pp)

(define big-plot-ysep 4)

(define (double-frame pp)
  (frame pp))

(define (add-yticks pp)
  (ppict-do
    pp
    #:go (coord 1 0 'lc #:abs-x 2) @bodyrmlo{100%}))

(define (add-xticks pp)
  (ppict-do
    pp
    ;;#:go (coord 0 1 'lt) @bodyrmlo{1x}
    #:go (coord 1/4 1 'rt) @bodyrmlo{2x}
    ;;#:go (coord 3/4 1 'ct) @bodyrmlo{10x}
    #:go (coord 1 1 'rt) @bodyrmlo{20x}))

(define (big-overhead-pict pre-pi* ds
                           #:wh [wh #f]
                           #:legend [legend? #f])
  (define pp (overhead-pict pre-pi* ds #:wh wh))
  (if legend?
    (add-yticks (add-xticks (double-frame pp)))
    (double-frame pp)))

(define (deco->sampling-pi bm-name d)
  (case d
    ((D)
     (performance-info->sample-info ((deco->pi bm-name) d)))
    (else (raise-argument-error 'deco->sampling-pi "(or/c 'D)" d))))

(define (ss-validate-plot pi w h)
  ;; TODO broken for RP data??!
  (define sample-pi (performance-info->sample-info pi #:replacement? #f))
  (define pp
    (with-plot-params w h base-color
      (validate-samples-plot pi sample-pi)))
  pp)

(define (big-sampling-plot bm-name deco #:wh [wh #f] #:legend? [legend? #t])
  (define pi ((deco->pi bm-name) deco))
  (define num-samples 10)
  (define sample-rate 10)
  (define num-units (performance-info->num-units pi))
  (define-values [w h]
    (if wh
      (values (car wh) (cdr wh))
      (values (w%->pixels 38/100) (h%->pixels 16/100))))
  (define pp (double-frame (ss-validate-plot pi w h)))
  (if legend?
    (add-xticks pp)
    pp))

(define DEEP-TAG 'deep-tag)
(define SHALLOW-TAG 'deep-tag)

(define (quote-para CLAIM-W . str*)
  (apply para #:width CLAIM-W
              #:align 'left
              #:fill? #true
              str*))

(define (make-gt-claim venue
                       year
                       author*
                       #:strategy dsu
                       #:url url
                       #:page page-num
                       body)
  (define pp (browncs-box body))
  (cloud-background pp))

(define (string*->text str*)
  (left-line-append*
    (map bodyrmlo str*)))

(define (cloud-background pp)
  (cc-superimpose
    (cloud (* 1.2 (pict-width pp))
           (* 1.2 (pict-height pp))
           happy-cloud-color
           #:style '(wide square))
    pp))

(define (bridge-append p0 p1)
  (ht-append med-y-sep p0 p1))

(define (bridge-pict pp0 pp1)
  (bridge-append (untyped-codeblock pp0) (typed-codeblock pp1)))

(define (title-block str)
  (titlerm str))

(define (tu-icon)
  (scale
    (bridge-pict2 #:sep small-x-sep
                  (typed-icon #:lbl #f)
                  (untyped-icon #:lbl #f))
    75/100))

(define (bridge-pict2 tt uu #:sep [sep #f])
  (let* ((pp (ht-append (or sep big-x-sep) (add-hubs tt 'TT) (add-hubs uu 'UU)))
         (pp (add-code-arrow
               pp
               (code-arrow 'TT-E rc-find 'UU-W lc-find 0 0 0 0 'solid)
               #:both #true)))
    pp))

(define (answer-pict pp)
  (answer-box
    (answer-text pp)))

(define (challenge-pict pp)
  (question-box
    (challenge-text pp)))

(define (question-pict pp)
  (question-box
    (question-text pp)))

(define (answer-text pp)
  (word-append
    @bodyembf{A} @bodyrm{.  } pp))

(define (challenge-text pp)
  (word-append
    @bodyembf{Challenge} @bodyrmlo{: } pp))

(define (question-text pp)
  (word-append
    @bodyrmhi{Q.  } pp))

(define (question-box pp)
  (browncs-box #:x-margin med-x-sep #:y-margin small-y-sep pp))

(define answer-box browncs-box)

(define (scale-lang-sidebar pp)
  (scale pp 80/100))

(define (scale-lang-sidebar2 pp)
  (scale pp 60/100))

(define (maybe-lbl do-lbl? pp name)
  (if do-lbl?
    (cc-superimpose pp (browncs-box (bodyembf name)))
    pp))

(define (interleave x* y*)
  (cond
    [(null? x*) y*]
    [(null? y*) x*]
    [else (list* (car x*) (car y*) (interleave (cdr x*) (cdr y*)))]))

(define (tr-pict)
  (symbol->lang-pict 'racket))

(define (ts-pict)
  (symbol->lang-pict 'typescript))

(define (flow-pict)
  (symbol->lang-pict 'flow))

(define (mypy-pict)
  (symbol->lang-pict 'python))

(define (typed-clojure-pict)
  (symbol->lang-pict 'clojure))

(define (pyre-pict)
  (symbol->lang-pict 'pyre))


(define (text->lang-pict str)
  (scale (headrm str) 80/100))

(define (retic-pict)
  (python-pict))

(define (actionscript-pict)
  (blank))

(define (cl-pict)
  (blank))

(define (hack-pict)
  (blank))

(define (pytype-pict)
  (blank))

(define (pyright-pict)
  (blank))

(define (rdl-pict)
  (blank))

(define (strongtalk-pict)
  (blank))

(define (typescript-pict)
  (blank))

(define (typed-lua-pict)
  (blank))

(define (gradualtalk-pict)
  (blank))

(define (grift-pict)
  (blank))

(define (tpd-pict)
  (blank))

(define (pyret-pict)
  (blank))

(define (grace-pict)
  (blank))

(define (pallene-pict)
  (blank))

(define (sp-pict)
  (blank))

(define (csharp-pict)
  (blank))


(define (dart2-pict)
  (blank))

(define (nom-pict)
  (blank))

(define (safets-pict)
  (blank))

(define (tsstar-pict)
  (blank))


(define (sorbet-pict)
  (blank))

(define (strongscript-pict)
  (blank))

(define (thorn-pict)
  (blank))

(define (indiana-pict)
  (symbol->lang-pict 'indiana))

(define (vss-pict)
  ;; TODO show Vitousek and coauthors??
  (blank))

(define (python-pict)
  (symbol->lang-pict 'python))

(define (example-lattice-n n #:x [x #f])
  (let ()
    (define-values [tu-pict pre-lattice-pict] (what-to-measure-lattice n #:x x))
    (values pre-lattice-pict)))

(define example-lattice-4 (example-lattice-n 3))
(define example-lattice-4x (example-lattice-n 3 #:x #true))

(define (lattice-to-y/n-pict)
  (ppict-do
    (hc-append
      tiny-x-sep
      (scale-to-width% example-lattice-4 35/100)
      (blank)
      (scale-to-width% example-lattice-4x 35/100))
    #:go (coord 53/100 1/2 'cc) right-arrow-pict))

(define (explain-oplot n)
  (hc-append
    small-x-sep
    (vc-append
      small-y-sep
      @bodyrmlo{N components => 2^N configurations}
      (scale-to-width% (if (< n 2) example-lattice-4 example-lattice-4x) 45/100))
    ((if (< n 1) bghost values)
     (overhead-grid (list (car ds-benchmark*))
                    (if (< n 2) render3 render4)))))

(define (pict-w-sup . pp*)
  (pict-w-sup* pp*))

(define (pict-w-sup* pp*)
  (define w (apply max (map pict-width pp*)))
  (for/list ((pp (in-list pp*)))
    (ct-superimpose (xblank w) pp)))

(define (gold-star)
  (make-simple-flag
    (blank tiny-x-sep pico-y-sep)
    #:flag-border-color dark-orange
    #:flag-border-width 4
    ;; #:flag-brush-style 'solid
    #:flag-background-color lite-orange))

(define (arrow-bullet pp)
  (hc-append
    tiny-x-sep
    right-arrow-pict pp))

(define (plus-bullet pp)
  (word-append @bodyrmhi{+ } pp))

(define (minus-bullet pp)
  (word-append @bodyrmhi{- } pp))

(define (label-below base . pp*)
  (vc-append 0 base (apply vc-append 2 pp*)))

(define (label-right base . pp*)
  (hc-append tiny-x-sep base (apply vc-append 2 pp*)))

(define (label-left base . pp*)
  (hc-append tiny-x-sep (apply vc-append 2 pp*) base))

(define (label-above base . pp*)
  (vc-append 0 (apply vc-append 2 pp*) base))

(define (X2-analysis-stack . sym*)
  (apply
    vr-append
    pico-x-sep
    (map (lambda (sym)
           (ht-append pico-x-sep
                      (coderm (symbol->string sym))
                      (scale (big-overhead-pict (list sym) 'D2 #:legend #f) 8/10)))
         sym*)))

(define (X-analysis-stack . pp*)
  (apply
    vl-append
    pico-y-sep
    (map (lambda (pp) (scale pp 8/10)) pp*)))

(define (venue->pict3 vv)
  (define pp0 (venue->pict2 "In Submission'22"))
  (define pp1 (venue->pict2 vv))
  (rc-superimpose (xblank (pict-width pp0)) pp1))

(define (venue->pict2 vv)
  (if (pict? vv)
    vv
    (scale (browncs-box (coderm vv)) 8/10)))

(define (venue->pict vv)
  (memory-bg (coderm vv)))

(define (venue*->pict descr title)
  (hc-append
    tiny-x-sep
    (bodyrmlo descr)
    (if title
      (coderm (string-append "[" title "]"))
      (blank))))

(define (se-arrow-pict)
  (scale (thick-right-arrow small-x-sep) 4/10))

(define (tight-grid . pp*)
  (tight-grid* pp*))

(define (tight-grid* pp*)
  (cond
    [(null? pp*)
     (blank)]
    [(null? (cdr pp*))
     (car pp*)]
    [else
      (vc-append pico-y-sep (hc-append pico-x-sep (car pp*) (cadr pp*)) (tight-grid* (cddr pp*)))]))


(define (benchmark-pict img #:w% [w% #f] #:lbl [lbl-above? #t] #:url [url #f])
  (define pp (browncs-box #:x-margin pico-y-sep #:y-margin pico-y-sep
                          (scale-to-square (bitmap img) (w%->pixels (or w% 30/100)))))
  (if url ((if lbl-above? label-above label-below) pp (coderm url) (yblank 4)) pp))

(define (benchmark-pict2 img #:url [url #f])
  (add-lite-bg
    (benchmark-pict img #:w% 28/100 #:lbl #f #:url url)))

(define (benchmark-pict3 img #:w% [w% #f] #:url [url #f])
  (add-lite-bg
    #:x tiny-y-sep
    #:y tiny-y-sep
    (label-below
      (browncs-box
        #:x-margin pico-y-sep #:y-margin pico-y-sep
        (scale-to-fit (bitmap img)
                      (w%->pixels (or w% 3/10))
                      600))
      (yblank pico-y-sep)
      (coderm url))))

(define (add-lite-bg pp #:x [x #f] #:y [y #f])
  (browncs-box pp #:color lite-grey #:x-margin x #:y-margin y))

(define memory-bg add-lite-bg)

(define (sunny-bg pp #:x [x #f] #:y [y #f])
  (browncs-box pp #:color shallow-brush-color #:x-margin x #:y-margin y))

(define (the-perf-problem)
  (bad-news
    (vc-append
      tiny-y-sep
      (word-append
        @bodyrmlo{Guarded}
        @bodyrmlo{ gradual types are too slow})
      @bodyembf{What to do?})))

(define (perf-what-to-do n #:only [only-show #f])
  (let* ((bb (browncs-box (blank (w%->pixels 40/100) (h%->pixels 16/100))))
         (task* '("Improve the compiler" "Remove checks statically" "Build a new compiler" "Use weaker types"))
         ;;(venue** '(("OOPSLA'18") ("POPL'18" "POPL'21") ("OOPSLA'17") ("Today!")))
         (venue** '(("Collapsible Contracts" "OOPSLA'18")
                    ("Corpse Reviver" "POPL'21")
                    ("Pycket" "OOPSLA'17")
                    ("Today!" #f)))
         )
    (table2
      #:col-sep small-x-sep
      #:row-sep small-y-sep
      (for/list ((task (in-list task*))
                 (venue* (in-list venue**))
                 (i (in-naturals)))
        (define n-pict (word-append (bodyrmhi (number->string (+ i 1))) @bodyrmlo{. }))
        (define t-pict (bodyembf task))
        (define v-pict (apply venue*->pict venue*))
        ((if (if only-show
               (= only-show i)
               (< i n))
           values bghost)
         (ppict-do
           bb
           #:go (coord 0 0 'lt #:abs-x tiny-x-sep #:abs-y tiny-y-sep)
           (vl-append pico-y-sep
             (word-append n-pict t-pict)
             (word-append (bghost n-pict) v-pict))))))))

(define (tu-bg pp #:x-margin [x-margin small-x-sep] #:y-margin [y-margin small-y-sep])
  pp)

(define (codebase-example n)
  ;; 0 = untyped, 1 = typed
  (scale (boundary-node (list 'U 'U (if (< 0 n) 'T 'U) 'U 'U) #:arrow #t) 6/10))

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

(define ds-benchmark* '(synth take5 quadU jpeg suffixtree dungeon #;fsmoo))

(define (overhead-grid bm* mk-pict)
  (define ww (* 34/100 client-w))
  (define hh (* 12/100 client-h))
  (define-values [ll* rr*] (split/2 bm*))
  (define (vapp pp*) (vl-append pico-y-sep (blank) (apply vl-append small-y-sep pp*) (blank)))
  (define mk-title
    (let* ((max-w (apply max (map (compose1 string-length symbol->string) bm*)))
           (base-pp (bghost (coderm (make-string max-w #\X)))))
      (lambda (left? sym)
        ((if left? rt-superimpose lt-superimpose) base-pp (coderm (symbol->string sym))))))
  (define (mk* left? bm*)
    (for/list ((bm (in-list bm*)))
      (define title (mk-title left? bm))
      (define pp (mk-pict bm ww hh))
      (apply ht-append pico-x-sep (if left? (list title pp) (list pp title)))))
  (ht-append
    small-x-sep
    (vapp (mk* #t ll*))
    (vapp (mk* #f rr*))))

(define (render1 bm ww hh)
  (render-overhead (list bm transient-rkt-version) ww hh))

(define (render2 bm ww hh)
  (render-overhead (list bm transient-rkt-version stransient) ww hh))

(define (render-overhead bm-name+v* ww hh)
  ;; TODO
  (blank)

  #;
  (parameterize ([*OVERHEAD-MAX* 20]
                 [*OVERHEAD-LEGEND?* #false]
                 [*OVERHEAD-PLOT-WIDTH* ww]
                 [*OVERHEAD-PLOT-HEIGHT* hh]
                 [*OVERHEAD-LINE-WIDTH* 3]
                 (*OVERHEAD-SHOW-RATIO* #f)
                 (*AUTO-POINT-ALPHA?* #f)
                 (*MULTI-INTERVAL-ALPHA* overhead-alpha)
                 [*INTERVAL-ALPHA* overhead-alpha]
                 #;[*OVERHEAD-LINE-COLOR* base-color]
                 [*PEN-COLOR-CONVERTER* pen-color-converter]
                 [*BRUSH-COLOR-CONVERTER* brush-color-converter]
                 ;[x-axis-ticks? #f]
                 ;[y-axis-ticks? #f]
                 )
    (define bm-name (car bm-name+v*))
    (define bm-v* (cdr bm-name+v*))
    (define pi* (for/list ((v (in-list bm-v*)))
                  (benchmark-name->performance-info bm-name v #:full-name? #t)))
    (add-xticks2
      (browncs-box
        #:x-margin 2
        #:y-margin 2
        (overhead-plot pi*)))))

(define (add-xticks2 pp)
  (define (mkc x y)
    (coord x y 'ct #:abs-y 2))
  (define base
    (ppict-do
      pp
      #:go (mkc 0 1) @tcoderm{1x}
      #:go (mkc 1 1) @tcoderm{20x}))
  (define maxw (+ 3 1/4))
  (define hh (pict-height pp))
  (for/fold ((acc base))
            ((ww (in-list '(3/4 3/2 5/2)))
             (nn (in-list '(  2   4  10))))
    (ppict-do
      acc
      #:go (coord (/ ww maxw) 1 'cb) (vrule hh #:thickness 1 #:color browncs-frame-color)
      #:go (mkc (/ ww maxw) 1) (tcoderm (format "~ax" nn)))))

(define (render3 . arg*)
  (define pp (apply render1 arg*))
  (ppict-do
    pp
    #:go (coord 0 0 'rc #:abs-x -2) @tcoderm{100%}))

(define (render4 . arg*)
  (define pp (apply render3 arg*))
  (define hh (pict-height pp))
  (ppict-do
    pp
    #:go (coord (/ 5/2 13/4)  1 'cb)
    (vrule hh #:thickness 4 #:color lite-orange)))

(define (deep-perf-pict)
  (overhead-grid ds-benchmark* render1))

(define (ds-perf-pict)
  (overhead-grid ds-benchmark* render2))

(define (cache-pict fn)
  (define filename (build-path "img" "cache" (format "~a.png" (object-name fn))))
  (unless (file-exists? filename)
    (save-pict filename (fn)))
  (bitmap filename))

(define (basic-example lhs rhs #:lbl? [lbl? #f] #:arrow? [arrow? #false] #:text? [text? #true])
  (let* ((pp
          (add-hubs
            ((dyn-codeblock lhs)
             #:title "Typed Function"
             "function add1(n : Num)"
             "  n + 1")
            'BT))
         (rhs-pict
               (add-hubs
                ((dyn-codeblock 'U)
                 #:title "Untyped Caller"
                 "add1(\"hola\")")
                'BB))
         (pp
           (vc-append tiny-y-sep
                              pp (if rhs rhs-pict (bghost rhs-pict))))
         (arr
           (code-arrow 'BR-S cb-find 'BL-W lc-find (* 3/4 turn) (* 15/100 turn) 90/100 5/100 'solid))
         (pp (if arrow? (add-code-arrow pp arr) pp))
         (lbl
             (table2
               #:row-sep tiny-y-sep
               #:col-sep tiny-x-sep
               (list
                 (ppict-do
                   ((if (symbol? lbl?) bghost values) y-str)
                   #:go (coord 0 1/2 'rc)
                   (if (eq? lbl? 'gte)
                     (word-append
                       (bodyembf natural-str)
                       @bodyrmlo{ and }
                       (bodyembf transient-str))
                     (blank)))
                 (answer-box @coderm{Error: expected Num})
                 (ppict-do
                   ((if (symbol? lbl?) bghost values) n-str)
                   #:go (coord 0 1/2 'rc)
                   (if (eq? lbl? 'gte)
                     (word-append
                       (bodyembf erasure-str))
                     (blank)))
                 (answer-box @coderm{"hola" + 1}))))
         (pp
           (ppict-do
             pp
             #:go (coord 0 1 'lt #:abs-y small-y-sep)
             (if lbl? lbl (bghost lbl))))
         )
    pp))

(define (bad-array-example #:lbl? [lbl? #f] #:cost? [cost? #f] #:retic? [retic? #f])
  (let* ((pp
          (vc-append
            tiny-y-sep
            ((dyn-codeblock 'U)
             #:title "Untyped Array"
             "arr = [\"A\", 3]")
            ((dyn-codeblock 'T)
             #:title "Typed Client"
             "nums : Array(Num) = arr"
             "nums[0]")))
         (lbl
             (table2
               #:row-sep tiny-y-sep
               #:col-sep tiny-x-sep
               (list
                 (ppict-do
                   ((if (symbol? lbl?) bghost values) y-str)
                   #:go (coord 0 1/2 'rc)
                   (if (eq? lbl? 'gte)
                     (word-append
                       (bodyembf natural-str)
                       @bodyrmlo{ and }
                       (bodyembf transient-str))
                     (blank)))
                 (answer-box @coderm{Error: expected Array(Num)})
                 (ppict-do
                   ((if (symbol? lbl?) bghost values) n-str)
                   #:go (coord 0 1/2 'rc)
                   (if (eq? lbl? 'gte)
                     (word-append
                       (bodyembf erasure-str))
                     (blank)))
                 (answer-box @coderm{"A"}))))
         (pp
           (ppict-do
             pp
             #:go (coord 0 1 'lt #:abs-y small-y-sep)
             (if lbl? lbl (bghost lbl))))
         (cost-pict
             (ll-append
               @headrm{How to Detect an Error?}
               (apply
                 vl-append
                 4
                 (filter
                   values
                   (list
                     (word-append
                       @bodyrmlo{    1. } ((if retic? bodyrmlo bodyrmhi) "Traverse") @bodyrmlo{ array at the boundary})
                     (word-append
                       @bodyrmlo{    2. } ((if retic? bodyrmlo bodyrmhi) "Wrap") @bodyrmlo{ array, check at reads})
                     (and
                       retic?
                       (word-append
                         @bodyemrm{    3. Check reads in typed code})))))
               (yblank tiny-y-sep)
               (if retic?
                 (blank)
                 (word-append
                   @bodyrmlo{Either way, } @bodyrmhi{costs} @bodyrmlo{ can add up!}))))
         (pp
           (if cost?
             (ht-append
               med-x-sep
               pp
               ((if (eq? 'ghost cost?) bghost values) cost-pict))
             pp))
         )
    pp))

(define (bad-array-example2 #:lbl? [lbl? #f] #:cost? [cost? #f] #:retic? [retic? #f])
  (let* ((pp
          (vc-append
            tiny-y-sep
            ((dyn-codeblock 'U)
             #:title "Untyped Array"
             "arr = [\"A\", 3]")
            ((dyn-codeblock 'T)
             #:title "Typed Interface"
             "nums : Array(Num) = arr")
            ((dyn-codeblock 'U)
             #:title "Unyped Client"
             "nums[0]")
            ))
         (lbl
             (table2
               #:row-sep tiny-y-sep
               #:col-sep tiny-x-sep
               (list
                 (ppict-do
                   ((if (symbol? lbl?) bghost values) y-str)
                   #:go (coord 0 1/2 'rc)
                   (if (eq? lbl? 'gte)
                     (bodyembf natural-str)
                     (blank)))
                 (answer-box @coderm{Error: expected Array(Num)})
                 (ppict-do
                   ((if (symbol? lbl?) bghost values) n-str)
                   #:go (coord 0 1/2 'rc)
                   (if (eq? lbl? 'gte)
                     (word-append
                       (bodyembf transient-str)
                       @bodyrmlo{ and }
                       (bodyembf erasure-str))
                     (blank)))
                 (answer-box @coderm{"A"}))))
         (pp
           (ppict-do
             pp
             #:go (coord 0 1 'lt #:abs-y small-y-sep)
             (if lbl? lbl (bghost lbl))))
         (cost-pict
             (ll-append
               @headrm{How to Detect an Error?}
               (apply
                 vl-append
                 4
                 (filter
                   values
                   (list
                     (word-append
                       @bodyrmlo{    1. } ((if retic? bodyrmlo bodyrmhi) "Traverse") @bodyrmlo{ array at the boundary})
                     (word-append
                       @bodyrmlo{    2. } ((if retic? bodyrmlo bodyrmhi) "Wrap") @bodyrmlo{ array, check at reads})
                     (and
                       retic?
                       (word-append
                         @bodyemrm{    3. Check reads in typed code})))))
               (yblank tiny-y-sep)
               (if retic?
                 (blank)
                 (word-append
                   @bodyrmlo{Either way, } @bodyrmhi{costs} @bodyrmlo{ can add up!}))))
         (pp
           (if cost?
             (ht-append
               med-x-sep
               pp
               ((if (eq? 'ghost cost?) bghost values) cost-pict))
             pp))
         )
    pp))

(define (retic-perf n)
  (ppict-do
    (browncs-box
      (freeze
        (scale-to-width
          (bitmap "img/retic-perf.png")
          (w%->pixels 6/10))))
    #:go (coord 0 0 'lt #:abs-x tiny-y-sep #:abs-y tiny-y-sep)
    (bad-news
      (ll-append
        (word-append @bodyrmlo{<} @bodyrmhi{6} @bodyrmlo{x overhead for Transient})
        (word-append @bodyrmlo{<} @bodyrmhi{18} @bodyrmlo{x overhead for Transient with blame})))))

(define (tr-app-head)
  (hc-append tiny-x-sep (tr-pict) @bodyrmhi{Typed Racket}))

(define (tr-app-body [n 0])
  (xindent
    (vl-append
      2
      (arrow-bullet
        (word-append @bodyrmhi{Strong types} @bodyrmlo{: Type soundness}))
      (xindent
        @bodyrmlo{+ Complete monitoring})
      (yblank tiny-y-sep)
      (arrow-bullet
        (word-append
          @bodyrmhi{High overheads} @bodyrmlo{ are common}))
      (xindent
        @bodyrmlo{on the GTP Benchmarks})
      (yblank tiny-y-sep)
      (arrow-bullet
        (word-append
          @bodyrmhi{Worst cases} @bodyrmlo{: }
          @bodyembf3{25x} @bodyrmlo{, } @bodyembf3{1400x}))
      (yblank tiny-y-sep)
      )))

(define (retic-head)
  (let* ((txt-pict
          (ll-append
            @headrm{The Inspiration:}
            @headrm{   Reticulated Python}))
         (logo-pict
           (big-retic-logo))
         )
    (hc-append small-x-sep txt-pict logo-pict)))

(define (big-retic-logo)
           ;; vss-pict ?
          (ppict-do
            (let ((pp (indiana-pict)))
              (rt-superimpose
                (blank (* 1.6 (pict-width pp))
                       (* 1.8 (pict-height pp)))
                pp))
            #:go (coord 33/100 60/100 'cc)
            (retic-pict)))

(define (retic-tr-logos)
  (let* ((lp (big-retic-logo))
         (rp (cc-superimpose (bghost lp) (tr-pict)))
        )
    (values lp rp)))

(define (retic-vs-tr-pict w%)
  (define-values [lp rp] (retic-tr-logos))
  (hc-append
    lp
    (thick-right-arrow (w%->pixels w%))
    (xblank tiny-x-sep)
    rp))

(define (retic-vs-tr-types n)
  (define-values [lp rp] (retic-tr-logos))
  (define text->pict coderm)
  (define text->pict2 codeemrm2)
  (define text*->pict (lambda (str*) (map text->pict str*)))
  (define text*->pict2 (lambda (str*) (map text->pict2 str*)))
  (define text*->pict3 (lambda (str*) (map (compose1 bghost text->pict) str*)))
  (define rp* (text*->pict '("Dynamic" "Int" "Ref T" "T -> T" "Class { T ... }" ".... a few more")))
  (define pre-tr* '(("Any (the top type)")
                    ("Integer" "Natural")
                    ("(Vectorof T)" "(Vector T ...)")
                    ("(-> T ... (Values T ...))")
                    ("(Class T ...)")
                    ("(All X T)"
                     "(Union T ...)"
                     "(Rec X T)")
                    (".... many more")))
  (define n++ (- n 2))
  (define tr*
    (let loop ((str** pre-tr*)
               (curr-i 0))
      (cond
        [(null? str**)
         '()]
        [else
          (define f
            (cond
              [(< curr-i n++) text*->pict]
              [(= curr-i n++) text*->pict2]
              [else text*->pict3]))
          (append (f (car str**)) (loop (cdr str**) (+ curr-i 1)))])))
  (define add-bg
    (lambda (pp)
      (browncs-box pp)))
  (define (pict*->column tag pp*)
    ;; igrone tag
    (apply
      vl-append
      pico-y-sep
      (ppict-do (car pp*) #:go (coord 0 1 'rb) @coderm{T := })
      (cdr pp*)))
  (define lcol (vc-append lp ((if (<= 0 n) values bghost) (pict*->column 'A rp*))))
  (define rcol (vc-append rp ((if (< 0 n) values ghost) (pict*->column 'B tr*))))
  (ht-append
    big-x-sep
    (ct-superimpose
      (bghost (browncs-box @coderm{Typecheck + Elaborate}))
      lcol)
    rcol))

(define (retic-vs-tr-table n)
  (define-values [lp rp] (retic-tr-logos))
  (define text->pict coderm)
  (define rp* (map text->pict '("Typecheck + Elaborate" "Python")))
  (define tr* (map text->pict '("Expand" "Typecheck" "Guard Boundaries" "Optimize" "Racket")))
  (define add-bg
    (let* ((mx (apply max (append (map pict-width rp*) (map pict-width tr*))))
           (bg (xblank mx)))
      (lambda (pp) (cc-superimpose bg pp))))
  (define (pict*->column tag pp*)
        (dmigration-append*
          (for/list ((pp (in-list pp*))
                     (i (in-naturals)))
            (tag-pict (browncs-box (add-bg pp)) (tag-append tag i)))))
  (define lcol (vc-append lp ((if (< 0 n) values bghost) (pict*->column 'A rp*))))
  (define rcol (vc-append rp ((if (< 1 n) values ghost) (pict*->column 'B tr*))))
  (ppict-do
    (ht-append
      big-x-sep
      lcol
      rcol)
    #:go (at-find-pict 'B-2 #:abs-x (- tiny-x-sep) #:abs-y (* 2 tiny-y-sep))
    (if (< 2 n)
      (ppict-do
        (hilite-box (add-bg (text->pict "Insert Transient Checks")))
        #:go (coord 0 0 'rc #:abs-x (- small-x-sep) #:abs-y tiny-y-sep)
        (lc-append
          (word-append
            @bodyrmlo{(replace only the guard pass)})
          #;(word-append @bodyrmlo{with a traversal over typed code})
        ))
      (blank))))

(define (arrow-bullet* . pp*)
  (apply vl-append 2 (map arrow-bullet pp*)))

(define (bad-news pp)
  (browncs-box
    #:x-margin pico-y-sep #:y-margin pico-y-sep
    (question-box pp)))

(define (bb-box pp)
  (browncs-box
    #:x-margin pico-y-sep #:y-margin pico-y-sep
    (browncs-box pp)))

(define pplay-steps (make-parameter 10))

(define ((slide-assembler/background base-assembler make-rect) slide-title slide-vspace slide-pict)
  (define foreground-pict (base-assembler slide-title slide-vspace slide-pict))
  (define background-pict
    (let ((+margin (* 2 margin))
          (-margin (- margin)))
      (inset (make-rect (+ +margin client-w) (+ +margin client-h)) -margin)))
  (cc-superimpose background-pict foreground-pict))

(define (huge-program-example n)
  (case n
    ((0)
      (vc-append
        pico-y-sep
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        (boundary-node '(B U B U B U B U B U B U B U B U B U B))
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        (boundary-node '(B U B U B U B U B U B U B U B U B U B))
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        (boundary-node '(B U B U B U B U B U B U B U B U B U B))
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        (boundary-node '(B U B U B U B U B U B U B U B U B U B))
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        ))
    ((1)
      (vc-append
        pico-y-sep
        (boundary-node '(U B U B U B U B U B T B U B U B U B U))
        (boundary-node '(B U B U B U B U B U B U B U B U B U B))
        (boundary-node '(U B T B U B T B U B U B U B U B U B U))
        (boundary-node '(B U B U B T B T B T B U B U B U B U B))
        (boundary-node '(U B T B U B T B T B U B U B U B U B U))
        (boundary-node '(B U B U B U B T B U B T B U B U B U B))
        (boundary-node '(U B U B U B T B U B U B U B T B T B T))
        (boundary-node '(B U B T B U B U B U B U B U B U B U B))
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        )
    )
    ((2)
      (vc-append
        pico-y-sep
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        (boundary-node '(B U B U B U B U B U B U B U B U B U B))
        (boundary-node '(U B U B K B U B U B U B U B U B U B U))
        (boundary-node '(B U B U B U B U B U B U B U B U B U B))
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        (boundary-node '(B U B U B U B U B U B U B U B U B U B))
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        (boundary-node '(B U B U B U B U B U B U B U B U B U B))
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        )
      )
    ((3)
     (ppict-do
      (vc-append
        pico-y-sep
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        (boundary-node '(B U B U B T B U B U B U B U B U B U B))
        (boundary-node '(U B U B K B T B U B U B U B U B U B U))
        (boundary-node '(B U B T B T B U B U B U B U B U B U B))
        (boundary-node '(U B U B T B U B U B U B U B U B U B U))
        (boundary-node '(B U B T B U B U B U B U B U B U B U B))
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        (boundary-node '(B U B U B U B U B U B U B U B U B U B))
        (boundary-node '(U B U B U B U B U B U B U B U B U B U))
        )
       #:go (coord 34/100 32/100 'lt)
       (browncs-box
         (lc-append
           (word-append
             @bodyrmlo{When something goes wrong,})
           (word-append
             @bodyrmlo{ use } @bodyrmhibb{sound types}
             @bodyrmlo{ to } @bodyrmhi{pinpoint} @bodyrmlo{ the issue}))))
    )
    ))

(define (the-problem n)
  (define idea-pict
    (word-append
      @bodyrmlo{That's the idea ... but there's a } @bodyrmhi{major problem} @bodyrmlo{ ...}))
  (define prob-pict
    (word-append
      @bodyrmlo{Sound types are } @bodyrmhi{expensive!}))
  (lc-append
    idea-pict
    (yblank small-y-sep)
    (if (< n 1)
      (bghost prob-pict)
      (ppict-do
        prob-pict
        #:go (coord 1 0 'lt #:abs-x pico-x-sep)
        (clock-pict 100))
      )))

(define (roadblock-pict pp)
  (define stop
    (bitmap (stop-icon #:color halt-icon-color #:height 64 #:material plastic-icon-material)))
    (question-box
  (vc-append
    tiny-y-sep
    @headrm{Roadblock}
      (hc-append
        small-x-sep
        stop
        pp
        stop))))

(define (compiler-challenge-pict dd img what how)
  ((if (< dd 2) bb-box browncs-box)
    (vc-append
      pico-y-sep
      (if (pict? what) what (bodyrmlo what))
      img
      ((if (< dd 1) bghost values)
       (word-append
         @bodyrmlo{+ }
         (if (pict? how) how (bodyrmlo how))
         @bodyrmlo{ +}
         )))))

(define default-icon-height 64)

(define (type-challenge-pict n)
  (compiler-challenge-pict n
    (tr-types-pict)
    (word-append
      @bodyrmlo{Enforcing types})
    (word-append
      @bodyrmlo{Generalize tag checks to } @bodyrmhi{shape checks})))

(define (tr-types-pict)
  (define add-bg memory-bg)
  (hc-append
    tiny-x-sep
    (vc-append
      tiny-y-sep
      (add-bg @coderm{All})
      (add-bg @coderm{Rec}))
    (add-bg @coderm{Union})))

(define (opt-challenge-pict n)
  (compiler-challenge-pict n
    (opt-challenge-icon)
    "Optimizing typed code"
    "Trust only shapes"))

(define (opt-challenge-icon [hh default-icon-height])
  (bitmap (stopwatch-icon #:height hh)))

(define (expand-challenge-pict n)
  (compiler-challenge-pict n
    (expand-challenge-icon)
    "Navigating expanded code"
    (word-append
      @bodyrmlo{Typechecker must leave }
      @bodyrmhi{evidence})))

(define (expand-challenge-icon [hh default-icon-height])
  (bitmap (left-magnifying-glass-icon #:height hh)))

(define (cast-challenge-pict n)
  (compiler-challenge-pict n
    (cast-challenge-icon )
    "Recording downcasts"))

(define (cast-challenge-icon [hh default-icon-height])
    (bitmap (floppy-disk-icon #:height hh #:color "gold" #:material glass-icon-material)))

(define (cost-challenge-pict n)
  (compiler-challenge-pict n
    (cost-challenge-icon)
    "Minimizing costs"
    "E.g. reduce codegen"))

(define (cost-challenge-icon [hh default-icon-height])
  (bitmap (record-icon #:color "forestgreen" #:height hh #:material glass-icon-material)))

(define (expand-example-before)
  (typed-codeblock* (list
    @tcoderm{(for/sum ([byte (open-input-file "my.txt")])}
    @tcoderm{  byte)}
)))

(define (expand-example-after)
  (typed-codeblock* (list
@tcoderm{(define seq (make-seq (open-input-file "my.txt")))}
@tcoderm{(define (for-loop result pos)}
@tcoderm{  (if (not (seq.use-pos? pos))}
@tcoderm{    result}
@tcoderm{    (let ([byte (seq.get-val pos)])}
@tcoderm{      (for-loop (if (or (not seq.use-val?)}
@tcoderm{                        (seq.use-val? byte))}
@tcoderm{                  (+ result byte)}
@tcoderm{                  result)}
@tcoderm{                (seq.next-pos pos)))))}
@tcoderm{(for-loop 0 seq.init)}
)))

(define (expand-example n)
  (case n
    ((0)
     (expand-example-before))
    ((1)
     (vl-append
       tiny-y-sep
       (expand-example-before)
       (hc-append (xblank tiny-x-sep) down-arrow-pict)
       (expand-example-after)))
    (else
      (error 'die))))

(define (challenge-pict*)
  (define the-bar
    (hc-append
      (* 2 tiny-x-sep)
      (tr-types-pict)
      (vc-append
        (- tiny-y-sep)
        (cast-challenge-icon)
        (ht-append
          pico-x-sep
          (opt-challenge-icon)
          (expand-challenge-icon)))))
  (freeze (scale the-bar 8/10)))

(define (blame-illustration n)
  (let* ((u0 @coderm{λx. "B"})
         (t0 @coderm{f : Num -> Num})
         (g0 (blank (pict-width t0) (pict-height u0)))
         (add-bg (lambda (pp) (cc-superimpose g0 pp)))
         (x0 (bghost (untyped-codeblock* (list (add-bg g0)))))
         (t1 @coderm{f(2)})
         (c0 (add-hubs
               (untyped-codeblock* (list (add-bg (if (< n 1) g0 u0))))
               'C0))
         (c1 (add-hubs
               (typed-codeblock* (list (add-bg ((if (< n 1) bghost values) t0))))
               'C1))
         (c2 (add-hubs
               (untyped-codeblock* (list (add-bg g0)))
               'C2))
         (c3 (add-hubs
               (typed-codeblock* (list (add-bg ((if (< n 1) bghost values) t1))))
               'C3))
         (pp (vl-append
               small-y-sep
               (hc-append pico-x-sep c0 x0 c2 x0)
               (hc-append pico-x-sep (tag-pict x0 'arr-anchor) c1 x0 c3)))
         (pp (add-code-arrows
               pp
               #:color black
               (code-arrow 'C0-S cb-find 'C1-W lc-find (* 3/4 turn) 0 2/4 2/4 'solid)
               (code-arrow 'C1-N ct-find 'C2-W lc-find (* 1/4 turn) 0 1/4 1/4 'solid)
               (code-arrow 'C2-E rc-find 'C3-W lc-find 0 0 1 1 'solid)))
         (pp (if (< n 2)
               pp
               (ppict-do
                 pp
                 #:go (at-find-pict 'C3 rc-find)
                 (bitmap (x-icon #:height default-icon-height)))))
         (pp (if (< n 3)
               pp
               (add-code-arrow
                 pp
                 (code-arrow 'C3-E rc-find 'arr-anchor ct-find (* 95/100 turn) (* 10/100 turn) 3/10 1/10 'solid)
                 #:arrow-size (+ 4 default-arrow-size)
                 #:line-width (+ 2 default-line-width)
                 #:color blame-color)))
        )
    pp))

(define (blame-how n)
  (define lhs-text
    (lc-append
      (word-append
        @bodyembf{Guarded} @bodyrmlo{ wrappers can attach precise})
      (word-append
        @bodyrmlo{blame info to values})))
  (define rhs-text1
    (lc-append
      (word-append
        @bodyembf{Transient} @bodyrmlo{ has no wrappers, but})
      (word-append
        @bodyrmlo{keeps a } @bodyrmhi{global map on the side})))
  (define rhs-text2
    (ll-append
      rhs-text1
      @bodyrmlo{ }
      @bodyrmlo{... a large map gets expensive}))
  (ht-append
    small-x-sep
    lhs-text
    (vrule (max (pict-height lhs-text) (pict-height rhs-text2)))
    ((if (< n 1) bghost values)
     (if (< n 2) rhs-text1 rhs-text2))))

(define all-blame-data
  (list
    (list "kcfa" "1x" "4x" ">540x")
    (list "morsecode" "3x" "2x" ">250x")
    (list "sieve" "4x" "15x" ">220x")
    (list "snake" "8x" "12x" ">1000x")
    (list "suffixtree" "6x" "31x" ">190x")
    (list "tetris" "10x" "12x" ">720x")
    (list "acquire" "1x" "4x" "34x")
    (list "dungeon" "5x" "15000x" "75x")
    (list "forth" "6x" "5800x" "48x")
    (list "fsm" "2x" "2x" "230x")
    (list "fsmoo" "4x" "420x" "100x")
    (list "gregor" "2x" "2x" "23x")
    (list "jpeg" "2x" "23x" "38x")
    (list "lnm" "1x" "1x" "29x")
    (list "mbta" "2x" "2x" "37x")
    (list "quadT" "7x" "25x" "34x")
    (list "quadU" "8x" "55x" "320x")
    (list "synth" "4x" "47x" "220x")
    (list "take5" "3x" "44x" "33x")
    (list "zombie" "31x" "46x" "560x")
    (list "zordoz" "3x" "3x" "220x")))

(define (blame-table n)
  (define title* (list "Benchmark" @bodyembf{Transient} "Guarded"
                       (word-append @bodyembf{T} @bodyrmlo{+Blame})))
  (define (num->pict str)
    (define n (string->number
                (let ((ss (substring str 0 (sub1 (string-length str)))))
                  (if (eq? #\> (string-ref ss 0))
                    (substring ss 1)
                    ss))))
    (unless (real? n)
      (printf "DEAD ~a~n" str))
    ((if (<= n 6) codeemrm (if (< n 100) coderm codeembf)) str))
  (define (?bodyrmlo x)
    (if (pict? x) x (bodyrmlo x)))
  (define (title->pre-pict* rr)
    (list (bghost (bodyrmlo (first rr))) (?bodyrmlo (second rr)) (?bodyrmlo (fourth rr)) (?bodyrmlo (third rr))))
  (define (row->pre-pict* rr)
    (list (coderm (first rr)) (num->pict (second rr)) (num->pict (fourth rr)) (num->pict (third rr))))
  (define mask*
    (case n
      ((0 1)
       (lambda (pp*)
         (define-values [ll rr] (split-at pp* (- (length pp*) (- 2 n))))
         (append ll (map bghost rr))))
      (else
       (lambda (pp*)
         pp*))))
  (define ff (compose1 mask* row->pre-pict*))
  (apply
    ht-append
    (w%->pixels 7/100)
    (for/list ((row (in-list (split/n all-blame-data 11))))
      (table
        4
        (flatten
          (cons
            (mask* (title->pre-pict* title*))
            (map ff row)))
        (cons lc-superimpose rc-superimpose)
        cc-superimpose
        tiny-x-sep
        tiny-y-sep))))

(define mixed-best-data*
  (list
    (list "forth" "12%")
    (list "fsm" "38%")
    (list "fsmoo" "31%")
    (list "mbta" "19%")
    (list "morsecode" "25%")
    (list "zombie" "6%")
    (list "dungeon" "31%")
    (list "jpeg" "38%")
    (list "zordoz" "47%")
    (list "lnm" "66%")
    (list "suffixtree" "48%")
    (list "kcfa" "55%")
    (list "snake" "46%")
    (list "take5" "36%")
    (list "acquire" "64%")
    (list "tetris" "62%")
    ))

(define (mixed-best-table n)
  (define title* (list "Benchmark" @bodyembf{Best with D+S}))
  (define (num->pict str)
    (define n (string->number
                (let ((ss (substring str 0 (sub1 (string-length str)))))
                  (if (eq? #\> (string-ref ss 0))
                    (substring ss 1)
                    ss))))
    (coderm #;(if (<= n 6) codeemrm (if (< n 100) coderm codeembf)) str))
  (define (?bodyrmlo x)
    (if (pict? x) x (bodyrmlo x)))
  (define (title->pre-pict* rr)
    (list (bghost (bodyrmlo (first rr))) (?bodyrmlo (second rr))))
  (define (row->pre-pict* rr)
    (list (coderm (first rr)) (num->pict (second rr))))
  (define mask*
    (case n
      ((0 1)
       (lambda (pp*)
         (define-values [ll rr] (split-at pp* (- (length pp*) (- 2 n))))
         (append ll (map bghost rr))))
      (else
       (lambda (pp*)
         pp*))))
  (define ff (compose1 mask* row->pre-pict*))
  (apply
    ht-append
    (w%->pixels 7/100)
    (for/list ((row (in-list (split/n mixed-best-data* 11))))
      (table
        2
        (flatten
          (cons
            (mask* (title->pre-pict* title*))
            (map ff row)))
        (cons lc-superimpose rc-superimpose)
        cc-superimpose
        tiny-x-sep
        tiny-y-sep))))

(define overhead-data*
  (list
    (list "sieve" "16x" "4.36x" "2.97x")
    (list "forth" "5800x" "5.51x" "5.43x")
    (list "fsm" "2.24x" "2.38x" "1.91x")
    (list "fsmoo" "420x" "4.28x" "4.25x")
    (list "mbta" "1.91x" "1.74x" "1.71x")
    (list "morsecode" "1.57x" "2.77x" "1.3x")
    (list "zombie" "46x" "31x" "31x")
    (list "dungeon" "15000x" "4.97x" "3.16x")
    (list "jpeg" "23x" "1.66x" "1.56x")
    (list "zordoz" "2.63x" "2.75x" "2.58x")
    (list "lnm" "1.23x" "1.21x" "1.17x")
    (list "suffixtree" "31x" "5.8x" "5.8x")
    (list "kcfa" "4.33x" "1.24x" "1.24x")
    (list "snake" "12x" "7.67x" "7.61x")
    (list "take5" "44x" "2.99x" "2.97x")
    (list "acquire" "4.22x" "1.42x" "1.42x")
    (list "tetris" "13x" "9.93x" "5.44x")
    (list "synth" "47x" "4.2x" "4.2x")
    (list "gregor" "1.72x" "1.59x" "1.51x")
    (list "quadT" "26x" "7.39x" "7.23x")
    (list "quadU" "55x" "7.57x" "7.45x")
    ))

(define (overhead-table n)
  (define title* (list "Benchmark" @bodyembf{Deep} @bodyembf{Shallow} @bodyembf{D||S}))
  (define (num->pict str)
    (define n (string->number
                (let ((ss (substring str 0 (sub1 (string-length str)))))
                  ss)))
    (unless (real? n)
      (printf "DEAD ~a~n" str))
    ((if (<= n 6) codeemrm (if (< n 100) coderm codeembf)) str))
  (define (?bodyrmlo x)
    (if (pict? x) x (bodyrmlo x)))
  (define (title->pre-pict* rr)
    (list (bghost (bodyrmlo (first rr)))
          (?bodyrmlo (second rr))
          (?bodyrmlo (third rr))
          (?bodyrmlo (fourth rr))))
  (define (row->pre-pict* rr)
    (list (coderm (first rr))
          (num->pict (second rr))
          (num->pict (third rr))
          (num->pict (fourth rr))))
  (define mask*
    (case n
      ((0 1)
       (lambda (pp*)
         (define-values [ll rr] (split-at pp* (- (length pp*) (- 2 n))))
         (append ll (map bghost rr))))
      (else
       (lambda (pp*)
         pp*))))
  (define ff (compose1 mask* row->pre-pict*))
  (apply
    ht-append
    (w%->pixels 7/100)
    (for/list ((row (in-list (split/n overhead-data* 11))))
      (table
        4
        (flatten
          (cons
            (mask* (title->pre-pict* title*))
            (map ff row)))
        (cons lc-superimpose rc-superimpose)
        cc-superimpose
        tiny-x-sep
        tiny-y-sep))))

(define path-data*
  (list
    (list "sieve" "0%" "0%" "100%")
    (list "forth" "0%" "0%" "50%")
    (list "fsm" "100%" "100%" "100%")
    (list "fsmoo" "0%" "0%" "50%")
    (list "mbta" "100%" "100%" "100%")
    (list "morsecode" "100%" "100%" "100%")
    (list "zombie" "0%" "0%" "50%")
    (list "dungeon" "0%" "0%" "67%")
    (list "jpeg" "0%" "100%" "100%")
    (list "zordoz" "100%" "100%" "100%")
    (list "lnm" "100%" "100%" "100%")
    (list "suffixtree" "0%" "0%" "12%")
    (list "kcfa" "33%" "100%" "100%")
    (list "snake" "0%" "0%" "0%")
    (list "take5" "0%" "100%" "100%")
    ))

(define (overhead-legend)
    (ht-append
      med-x-sep
      (word-append
        @bodyrmhi{x} @bodyrmlo{ axis = [1x, 20x]  (sets a limit for "fast enough")})
      (word-append
        @bodyrmhi{y} @bodyrmlo{ axis = % of all gradually-typed points})))

(define (transient-works n)
  ;; TODO tie back to RQ1 RQ2
  (define main-rq
    (ll-append
      (word-append
        @bodyrmhi{RQ} @bodyrmlo{. Can } @bodyembf{transient types} @bodyrmlo{:})
      @bodyrmlo{        - scale to a rich type system}
      @bodyrmlo{        - in the context of an existing compiler?}))
  (define y-pict @bodyembf{  Yes! })
  (define main-ans
      (word-append
        y-pict @bodyrmlo{... without blame}))
  (define caveats-1
    ;; TYPES?!
    (ll-append
      (word-append (bghost y-pict) @bodyrmlo{... and with some tailoring})
      #;(word-append (bghost y-pict) @bodyrmlo{    of the IR for new types})
      ))
  (table2
    #:col-align lc-superimpose
    #:row-align cc-superimpose ;; TODO
    #:col-sep small-x-sep
    #:row-sep tiny-y-sep
    (list
      main-rq (freeze (scale (retic-vs-tr-pict 1/10) 8/10))
      (blank) (blank)
      ((if (< n 1) bghost values) main-ans) (blank)
      ((if (< n 2) bghost values) caveats-1) ((if (< n 2) bghost values) (challenge-pict*)))))

(define (perf-bottom-line)
    (hc-append
      tiny-x-sep
      y-str
      (word-append
        @bodyrmlo{Overall performance is } @bodyrmhi{much improved})))

(define (optional-langs-pict)
    (hc-append
      tiny-x-sep
      (ts-pict)
      (flow-pict)
      (mypy-pict)
      (typed-clojure-pict)
      (pyre-pict)))

(define (transient-gateway n)
  (define spectrum-pict
    (browncs-box
      (hc-append
        tiny-x-sep
        (bodyembf natural-str)
        @headrm{>}
        (bodyembf transient-str)
        @headrm{<}
        (bodyembf erasure-str))))
  (define (lang-label img pp)
    (hc-append small-x-sep img pp))
  (define today-pict
    (values ;;lang-label
      ;; (tr-pict)
      (word-append
        @bodyrmlo{Reminder: }
        @bodyembf{Transient}
        @bodyrmlo{ is a promising way to }
        @bodyrmhi{strengthen}
        @bodyrmlo{ unsound }
        @bodyembf{Optional}
        @bodyrmlo{ types})))
  (define tomorrow-pict
    (lang-label
      (ts-pict)
      (word-append
        @bodyrmhi{Strenghtening} @bodyrmlo{ is possible too})))
  (vc-append
    small-y-sep
    today-pict
    spectrum-pict
    (blank)
    (vc-append
      4
      @bodyrmlo{Lots of potential clients!}
      (bb-box (optional-langs-pict)))
    )

  #;(hc-append
    med-x-sep
    spectrum-pict
    (vl-append
      small-y-sep
      today-pict
      tomorrow-pict)
    ))

(define (three-alt-pict [n 0])
  (define name* (list natural-str transient-str erasure-str))
  (define desc* '(("Types enforce" "behaviors")
                  ("Types enforce" "top-level shapes")
                  ("Types enforce" "nothing")))
  (define pp*
    (for/list ([nm (in-list name*)]
               [ds (in-list desc*)])
      (vc-append
        pico-x-sep
        (tag-pict (bodyembf nm) (string->symbol nm))
        (apply vc-append 2 (map bodyrmlo ds)))))
  (define pp (apply ht-append med-x-sep (map browncs-box pp*)))
  (case n
    ((0)
     pp)
    ((1)
     (ppict-do pp
       #:go (at-find-pict 'Guarded   lt-find 'rc #:abs-x (- pico-y-sep) #:abs-y pico-y-sep) (tr-pict)))
    ((2)
     pp
     #;(above-all-lang (browncs-box pp)))))

(define (region-pict name #:color color)
  (let* ((bg (filled-rounded-rectangle
               (w%->pixels 26/100)
               (h%->pixels 2/10)
               1
               #:color color
               #:border-color black
               #:border-width 1))
         (fg (bodyrm name))
         (pp (cc-superimpose bg fg))
         (sym (string->symbol name)))
    (add-hubs pp sym)))

(define interaction-y-sep med-y-sep)
(define interaction-x-sep (w%->pixels 24/100))

(define (tu-interaction n)
  (let* ((t-pict (region-pict "Typed" #:color typed-brush-color))
         (u-pict (region-pict "Untyped" #:color untyped-brush-color))
         (pp (hc-append interaction-x-sep t-pict u-pict))
         (arr (code-arrow 'Typed-E rc-find 'Untyped-W lc-find 0 0 0 0 'solid))
         (pp (if (< n 1)
               pp
               (add-code-arrow pp arr #:arrow-size large-arrow-size #:both #true))))
    pp))

(define (dsu-interaction n)
  (let* ((d-pict (region-pict "Deep Typed" #:color deep-brush-color))
         (s-pict (region-pict "Shallow Typed" #:color shallow-brush-color))
         (ds-pict (vc-append interaction-y-sep d-pict s-pict))
         (u-pict (region-pict "Untyped" #:color untyped-brush-color))
         (u+-pict (cc-superimpose (yblank (- (pict-height ds-pict) (pict-height d-pict))) u-pict))
         (pp (hc-append interaction-x-sep ds-pict u+-pict))
         (pp (if (< n 1)
               pp
               (add-code-arrow*
                 (add-code-arrow
                   pp
                   (code-arrow '|Deep Typed-S| cb-find '|Shallow Typed-N| ct-find (* 3/4 turn) (* 3/4 turn) 0 0 'solid)
                   #:arrow-size large-arrow-size #:both #true)
                 (list
                   (code-arrow u+-pict ct-find '|Deep Typed-E| rc-find (* 1/2 turn) (* 1/2 turn) 0 0 'solid)
                   (code-arrow u+-pict cb-find '|Shallow Typed-E| rc-find (* 1/2 turn) (* 1/2 turn) 0 0 'solid)
                   (code-arrow u+-pict ct-find '|Untyped-N| ct-find (* 1/2 turn) (* 3/4 turn) 0 0 'solid)
                   (code-arrow u+-pict cb-find '|Untyped-S| cb-find (* 1/2 turn) (* 1/4 turn) 0 0 'solid))
                 #:arrow-size large-arrow-size))))
    pp))


(define the-title-str "Deep and Shallow Types for Gradual Languages")

(define (sec:title)
  (pslide
    #:next
    #:go (coord 1/2 26/100 'ct)
    (let* ([title-pict
             (browncs-box
               #:y-margin small-y-sep
               (let* ((top the-title-str))
                 (title-block top)))]
           [tu-pict
             (vc-append
               pico-y-sep
               (boundary-node '(U D U D D))
               (boundary-node '(U U D D))
               #;(boundary-node '(D D U U D))
               )]
           [ben-pict (vr-append
                       -4
                       @subtitlerm{Ben Greenman}
                       (yblank 12)
                       @subtitlerm{2022-06-xx})]
           [brown-pict
                 (scale-to-width% (bitmap "img/browncs-logo.png") 14/100)]
           [author-pict (browncs-box (hc-append small-x-sep ben-pict brown-pict))])
      (vc-append
        tiny-y-sep
          (vc-append tiny-y-sep title-pict (bghost tu-pict))
        author-pict))
  )
  (void))

(define (sec:unsorted)
  (pslide
    #:go heading-coord-m
    @headrm{Gradual Landscape}
    #:go center-coord
    (let* ((lang*
             (list (actionscript-pict) (cl-pict) (mypy-pict)
                   (flow-pict) (hack-pict) (pyre-pict)
                   (pytype-pict) (pyright-pict) (rdl-pict)
                   (strongtalk-pict) (typescript-pict) (typed-clojure-pict)
                   (typed-lua-pict) (gradualtalk-pict) (grift-pict)
                   (tpd-pict) (tr-pict) (pyret-pict) (grace-pict)
                   (pallene-pict) (retic-pict) (sp-pict) (csharp-pict)
                   (dart2-pict) (nom-pict) (safets-pict) (tsstar-pict)
                   (sorbet-pict) (strongscript-pict) (thorn-pict)))
           (lang**
             (split/n lang* 5))
           (pp* (map (lambda (l*) (apply hc-append tiny-x-sep l*))))
           (pp (apply vc-append small-y-sep pp*)))
      pp)
  )
  (pslide
    @bodyrmlo{bottom line, no clear winners}
    @bodyrmlo{good clean-slate approaches ... if you can do that}
  )
  (pslide
    #:go (coord slide-text-left slide-text-top 'lt)
    (the-perf-problem)
    #:go (coord 1/2 slide-text-top 'ct)
    (bghost (the-perf-problem))
    (yblank small-y-sep)
    #:alt ( (perf-what-to-do 0) )
    #:alt ( (perf-what-to-do 1) )
    #:alt ( (perf-what-to-do 2) )
    #:alt ( (perf-what-to-do 3) )
    (perf-what-to-do 4)
  )
  (pslide
    ;; ground rules
    ;; - 
  )
  (pslide
    #:go heading-coord-m
    @headrm{before}
    #:go center-coord
    #:alt ( (tu-interaction 0) )
    (tu-interaction 1)
  )
  (pslide
    #:go center-coord
    @headrm{explain optional}
  )
  (pslide
    #:go center-coord
    @headrm{explain deep}
  )
  (pslide
    #:go center-coord
    @headrm{explain transient}
  )
  (pslide
    #:go center-coord
    @headrm{motivate both}
  )
  (pslide
    #:go heading-coord-m
    @headrm{after}
    #:go center-coord
    #:alt ( (dsu-interaction 0) )
    (dsu-interaction 1)
  )
  (pslide
    #:go heading-coord-l
    @headrm{... Mixed Best}
    #:next
    #:go (coord 1/2 slide-text-top 'ct)
    (mixed-best-table 2)
  )
  (pslide
    #:go heading-coord-l
    @headrm{Worst Case Overhead vs. Untyped}
    #:next
    #:go (coord 1/2 slide-text-top 'ct)
    ;; #:alt ( (overhead-table 0) )
    ;; #:alt ( (overhead-table 1) )
    (overhead-table 2)
  )
  ;; sec
  (pslide
    @bodyrm{conclusions}
  )


  (pslide
    ;; recruiting
    #:go center-coord
    (add-rounded-border
      (scale-to-fit (bitmap "img/flux-bg.jpeg") (* 95/100 client-w) (* 98/100 client-h))
      #:x-margin 1
      #:y-margin 1
      #:radius 0.1
      #:frame-width 1
      #:frame-color black)
    #:go center-coord
    (add-rounded-border
      (bitmap "img/the-u.png")
      #:x-margin small-x-sep
      #:y-margin tiny-y-sep
      #:radius 0.1
      #:frame-width 1
      #:frame-color black)
  )


  (void))

(define (sec:intro)
  (pslide
    #:go heading-coord-m
    @headrm{Context = Gradual Typing}
    #:go slide-text-coord-m
    (tag-pict (word-append
      @bodyrmhi{High-level goal}
      @bodyrmlo{: mix typed and untyped code}) 'top-text)
    #:next
    #:go hi-text-coord-m
    (yblank med-y-sep)
    (basic-example 'T 'U)
    #:next
    (yblank med-y-sep)
    (word-append
      @bodyrmhi{Central question}
      @bodyrmlo{: what should types mean at run-time?})
  )
  (pslide
    #:go heading-coord-l
    @headrm{What Should Types Mean?}
    #:go hi-text-coord-m
    @bodyrmlo{Three leading strategies:}
    #:next
    (yblank med-y-sep)
    (three-alt-pict 0)
  )
  (pslide
    #:go heading-coord-l
    @headrm{Example}
    #:go hi-text-coord-m
    (yblank med-y-sep)
    #:alt ( (basic-example 'T 'U))
    (basic-example 'T 'U #:lbl? 'gte)
  )
  (pslide
    #:go heading-coord-r
    @headrm{Example 2}
    #:go hi-text-coord-m
    (yblank med-y-sep)
    #:alt ( (bad-array-example) )
    (bad-array-example #:lbl? 'gte)
    #:go slide-text-coord-m
    (word-append
      (bodyembf natural-str)
      @bodyrmlo{ and }
      (bodyembf transient-str)
      @bodyrmlo{ agree, but for different reasons ...})
  )
  (pslide
    #:go heading-coord-r
    @headrm{Example 2+}
    #:go slide-text-coord-m
    (word-append
      (bodyembf natural-str)
      @bodyrmlo{ and }
      (bodyembf transient-str)
      @bodyrmlo{ agree, but for different reasons ...})
    (yblank tiny-y-sep)
    (word-append
      @bodyrmlo{ ... and they disagree for an untyped client})
    #:next
    #:go hi-text-coord-m
    (yblank med-y-sep)
    #:alt ( (bad-array-example2) )
    (bad-array-example2 #:lbl? 'gte)
  )
  (pslide
    #:go hi-text-coord-m
    (bghost @bodyrmlo{Three leading strategies:})
    (yblank med-y-sep)
    #:alt ( (three-alt-pict 0) )
    (three-alt-pict 1)
    #:go slide-text-coord-m
    (yblank tiny-y-sep)
    (word-append
      @bodyrmlo{Typed Racket has } @bodyembf{Guarded} @bodyrmlo{ types ... and a big problem})
  )
  (pslide
    #:go heading-coord-l
    @headrm{Guarded Types are Expensive!}
    #:next
    #:go hi-text-coord-l
    (memory-bg (vl-append tiny-y-sep (tr-app-head) (tr-app-body)))
    #:next
    #:go (coord 42/100 36/100 'lt)
    (bad-news @headrm{Q.   Is Sound Gradual Typing Dead?})
  )
  (pslide
    #:go (coord slide-text-left slide-text-top 'lt)
    (the-perf-problem)
    #:go (coord 1/2 slide-text-top 'ct)
    (bghost (the-perf-problem))
    (yblank small-y-sep)
    #:alt ( (perf-what-to-do 0) )
    #:alt ( (perf-what-to-do 1) )
    #:alt ( (perf-what-to-do 2) )
    #:alt ( (perf-what-to-do 3) )
    (perf-what-to-do 4)
  )
  (pslide
    #:go (coord 1/2 30/100 'ct)
    (lc-append
      (word-append
        @bodyrmlo{Hope to } @bodyrmhi{reduce costs across the board})
      (word-append
        @bodyrmlo{without changing the surface language}))
    (yblank tiny-y-sep)
    (arrow-bullet*
      @bodyrmlo{Same code, types, and type checker}
      (word-append
        @bodyrmlo{Different run-time behavior}))
    #:go (coord 1/2 slide-text-top 'ct)
    (bghost (the-perf-problem))
    (yblank small-y-sep)
    (perf-what-to-do 5 #:only 3)
  )

  (void))

(define (sec:rp)
  (pslide
    #:go heading-coord-m
    (retic-head)
    (yblank pico-y-sep)
    (lc-append
      (word-append
        @bodyembf{Transient}
        @bodyrmlo{ sematics ~ enforce types with tag checks})
      (word-append
        @bodyrmhi{No} @bodyrmlo{ contract wrappers}))
    (yblank med-y-sep)
    #:next
    (ppict-do
      (retic-perf 1)
      #:go (coord 2/100 0 'lc #:abs-y (- (* 1.6 tiny-y-sep)))
      (ppict-do
        @bodyrmlo{Performance is not bad!}
        #:go (coord 1 1/2 'lc #:abs-x tiny-x-sep)
        @coderm{[POPL'17]}))
  )
  (pslide
    #:go hi-text-coord-m
    @headrm{Research Questions}
    ;; TODO 2 goals = rich type system and compiler
    ;;  - can T work for rich
    ;;  - do so without rewriting compiler
    (yblank small-y-sep)
    (tag-pict
    (word-append
      @bodyrmhi{RQ0}
      @bodyrmlo{. How to add } @bodyembf{transient types}
      @bodyrmlo{ to Typed Racket?}) 'RQ0)
    (yblank tiny-y-sep)
    (retic-vs-tr-pict 20/100)
    #:next
    (yblank small-y-sep)
    (ll-append
      (word-append
        @bodyrmhi{RQ1}
        @bodyrmlo{. Can Transient } @bodyrmhi{scale}
        @bodyrmhi{ to a } @bodyrmhi{rich type system} @bodyrmlo{?})
      (yblank pico-y-sep)
      (word-append
        @bodyrmhi{RQ2}
        @bodyrmlo{. Can we } @bodyrmhi{adapt an existing complier} @bodyrmlo{ to do so?}))
    #:next
    #:go (at-find-pict 'RQ0 ct-find 'ct)
    (bb-box
      (vc-append
        small-y-sep
        (lc-append
          @bodyrmlo{Implications for other gradual languages,}
          (word-append
            @bodyrmlo{especially } @bodyembf{Optional} @bodyrmlo{ ones that wish to } @bodyrmhi{strengthen} @bodyrmlo{ their types}))
        (optional-langs-pict)))
  )
  (pslide
    #:go heading-coord-l
    @headrm{Two Type Systems}
    #:alt ( (retic-vs-tr-types -1) )
    #:alt ( (retic-vs-tr-types 1) )
    #:alt ( (retic-vs-tr-types 2) )
    #:alt ( (retic-vs-tr-types 3) )
    #:alt ( (retic-vs-tr-types 4) )
    #:alt ( (retic-vs-tr-types 5) )
    #:alt ( (retic-vs-tr-types 6) )
    #:alt ( (retic-vs-tr-types 7) )
    (retic-vs-tr-types 8)
  )
  (pslide
    #:go heading-coord-r
    @headrm{Two Compilers}
    #:go slide-text-coord-m
    #:alt ( (retic-vs-tr-table 0) )
    #:alt ( (retic-vs-tr-table 1) )
    #:alt ( (retic-vs-tr-table 2) )
    (retic-vs-tr-table 3)
  )
  (pslide
    #:go heading-coord-m
    (ppict-do
      @headrm{Challenges}
      #:go (coord 1/2 8/100 'ct)
      (scale (retic-vs-tr-pict 90/100) 8/10))
    #:next
    #:go (coord 24/100 40/100 'cc)
    #:alt ( (type-challenge-pict 0) )
    #:alt ( (type-challenge-pict 1) )
    (type-challenge-pict 2)
    #:go (coord 33/100 80/100 'cc)
    #:alt ( (opt-challenge-pict 0) )
    #:alt ( (opt-challenge-pict 1) )
    (opt-challenge-pict 2)
    #:go (coord 74/100 33/100 'cc)
    #:alt ( (expand-challenge-pict 0) )
    #:alt ( (expand-challenge-pict 1) )
    (expand-challenge-pict 2)
;    #:go (coord 56/100 47/100 'lt)
;    #:alt ( (cast-challenge-pict 0) )
;    (cast-challenge-pict 1)
    #:go (coord 72/100 70/100 'cc)
    #:alt ( (cost-challenge-pict 0) )
    #:alt ( (cost-challenge-pict 1) )
    (cost-challenge-pict 2)
  )
  (pslide
    #:go (coord slide-right slide-heading-top 'rt)
    (expand-challenge-pict 2)
    #:next
    #:go slide-text-coord-l
    #:alt ( (expand-example 0) )
    (expand-example 1)
    #:next
    #:go center-coord
    (browncs-box
      #:x-margin small-x-sep
      #:y-margin small-y-sep
      (word-append
        @bodyrmlo{Don't want to check } @bodyrmhi{every} @bodyrmlo{ function call!}))
  )

  (void))

(define (sec:noblame-perf)
  (pslide
    #:go heading-coord-m
    (ppict-do
      (bghost @headrm{Challenges})
      #:go center-coord y-str
      #:go (coord 1/2 8/100 'ct)
      (scale (retic-vs-tr-pict 90/100) 8/10))
    #:next
    #:go center-coord
    @bodyrmlo{How's performance?}
    (yblank med-y-sep)
    (ppict-do
      (retic-perf 1)
      #:go (coord 1/2 0 'cb #:abs-y (- (* 1.0 tiny-y-sep)))
      @bodyrmlo{Does it match the worst cases for Reticulated?})
  )
  (pslide
    #:go heading-coord-l
    @headrm{Worst Case Overhead vs. Untyped}
    #:next
    #:go (coord 1/2 slide-text-top 'ct)
    #:alt ( (blame-table 0) )
    (blame-table 1)
    #:next
    #:go center-coord
    (browncs-box
      (vc-append
        small-y-sep
        (blank)
        (word-append
          @bodyembf{Transient} @bodyrmlo{ alone is } @bodyembf2{not so bad})
        (word-append
          @bodyembf{T} @bodyrmlo{+Blame gets } @bodyembf3{expensive})
        (blank)))
  )
  (pslide
    #:go heading-coord-m
    @headrm{Blame: The Idea}
    #:go heading-coord-m
    (bghost @headrm{B})
    (yblank small-y-sep)
    #:alt ( (blame-illustration 0) )
    #:alt ( (blame-illustration 2) )
    (blame-illustration 3)
    (yblank small-y-sep)
    (lc-append
      (word-append
        @bodyrmlo{When a } @bodyemty{typed} @bodyrmlo{/}
        @bodyemun{untyped} @bodyrmlo{ interaction goes wrong,})
      (word-append
        @bodyembl{blame} @bodyrmlo{ shows where to start debugging}))
    #:next
    (yblank small-y-sep)
    #:alt ( (blame-how 0) )
    #:alt ( (blame-how 1) )
    (blame-how 2)
  )
  (pslide
    #:go heading-coord-l
    @headrm{Worst Case Overhead vs. Untyped}
    #:go (coord 1/2 slide-text-top 'ct)
    (blame-table 1)
    #:next
    #:go center-coord
    (browncs-box
      (vc-append
        small-y-sep
        (blank)
        (vc-append
          tiny-y-sep
          (word-append
            @bodyrmlo{Why is } @bodyembf{T} @bodyrmlo{+Blame so much worse than Reticulated?})
          (ll-append
            @bodyrmlo{1. Larger, longer-running benchmarks}
            @bodyrmlo{2. No dynamic type}))
        (blank)))
  )
  (pslide
    #:go center-coord
    (roadblock-pict
      (word-append
        @bodyembf{T}
        @bodyrmlo{+Blame is too expensive!}))
    (yblank tiny-y-sep)
    @bodyrmlo{Future: can run-time support reduce the cost?}
  )
  (pslide
    #:go heading-coord-r
    @headrm{Overall Performance}
    #:next
    #:go slide-text-coord-m
    (yblank tiny-y-sep)
    (word-append
      @bodyrmlo{Gradual types should support }
      @bodyrmhi{all}
      @bodyrmlo{ mixed-typed configurations})
    #:go center-coord
    #:alt ( (explain-oplot 0) )
    #:alt ( (explain-oplot 1) )
    (tag-pict (explain-oplot 2) 'plot)
    #:go (at-find-pict 'plot cb-find 'ct #:abs-y small-y-sep)
    (lc-append
      (word-append
        @bodyrmlo{At } @bodyemrm{x} @bodyrmlo{=10, count the % of configurations})
      (word-append
        @bodyrmlo{that run at most 10x slower than untyped}))
  )
  (pslide
    #:go heading-coord-r
    @headrm{Overall Performance}
    #:go heading-coord-m
    (bghost @headrm{Overall Performance})
    #:next
    (yblank tiny-y-sep)
    (word-append
      @bodyrmhibb{Guarded} @bodyrmlo{: % of fast-enough points})
    (yblank small-y-sep)
    (deep-perf-pict)
    (yblank small-y-sep)
    (overhead-legend)
  )
  (pslide
    #:go heading-coord-m
    @headrm{Overall Performance}
    (yblank tiny-y-sep)
    (word-append
      @bodyrmhibb{Guarded} @bodyrmlo{  vs  } @bodyrmhiyy{Transient}
      @bodyrmlo{: % of fast-enough points})
    (yblank small-y-sep)
    (ds-perf-pict)
    (yblank small-y-sep)
    (overhead-legend)
  )
  (pslide
    #:go heading-coord-m
    @headrm{Overall Performance}
    (yblank tiny-y-sep)
    (word-append
      @bodyrmhibb{Guarded} @bodyrmlo{  vs  } @bodyrmhiyy{Transient}
      @bodyrmlo{: % of fast-enough points})
    (yblank small-y-sep)
    (let* ((pp (ds-perf-pict))
           (ww (pict-width pp))
           (hh (pict-height pp))
           (mask-color (color%-update-alpha white 0.7))
           (mask-pict (lambda (w h) (filled-rectangle w h #:color mask-color #:draw-border? #f))))
      (lc-superimpose
        (rt-superimpose
          pp
          (mask-pict (* 1/2 ww) (* 2/3 hh)))
        (mask-pict (* 1/2 ww) hh)))
    (yblank small-y-sep)
    (overhead-legend)
    #:next
    #:go (coord 48/100 32/100 'ct)
    ;; TODO keep discussion short, consider adding more text to the slide
    (browncs-box
      (vc-append
        tiny-y-sep
        (word-append
          @bodyrmhiyy{Transient} @bodyrmlo{ ~ low costs in general})
        (word-append
          @bodyrmhibb{Guarded} @bodyrmlo{ ~ high cost, but only for interactions})
        (blank)
        @bodyrmlo{Future: systematically explore combinations}))
  )

  (void))

(define (sec:expr)
  (pslide
    #:go center-coord
    @bodyrmlo{expressiveness, what why}
  )
  (pslide
    #:go heading-coord-m
    @headrm{No Wrappers = Simpler}
    @bodyrmlo{any wrapper is do-nothing rather than allow-nothing}
    #:go hi-text-coord-m
    (ht-append
      med-x-sep
      (typed-codeblock* (list
@tcoderm{(define b : (Boxof Integer)}
@tcoderm{  (box 0))}
@tcoderm{}
@tcoderm{(define any : Any b)}))
      (untyped-codeblock* (list
@tcoderm{(set-box! any 1)}
)))
    (yblank small-y-sep)
    (table2
      #:col-sep small-x-sep
      #:row-sep tiny-y-sep
      (list
        @bodyembf{Guarded} @coderm{Error: cannot mutate an Any-wrapped value}
        @bodyembf{Transient} @coderm{OK}))
  )
  (pslide
    #:go heading-coord-m
    @headrm{No Missing Wrappers}
    @bodyrmlo{deep, at the moment, does not have wrappers for mutable pairs}
    #:go hi-text-coord-m
    (vc-append
      small-y-sep
      (typed-codeblock* (list
@tcoderm{(: add-mpair (-> (MPairof Integer Integer) Integer))}
@tcoderm{(define (add-mpair mp)}
@tcoderm{  (+ (mcar mp) (mcdr mp)))}
))
      (untyped-codeblock* (list
@tcoderm{(add-mpair (mcons 2 4))}
)))
    (yblank small-y-sep)
    (table2
      #:col-sep small-x-sep
      #:row-sep tiny-y-sep
      (list
        @bodyembf{Deep} @coderm{Error: no contract for type}
        @bodyembf{Shallow} @coderm{OK}))
  )
  (pslide
    #:go heading-coord-m
    @headrm{uniform behavior}
    @bodyrmlo{contract wrappers from deep types can change behavior, no wrap = no change}
    #:go hi-text-coord-m
    (vc-append
      small-y-sep
      (typed-codeblock* (list
@tcoderm{(require/typed racket/list}
@tcoderm{  [index-of}
@tcoderm{    (All (T)}
@tcoderm{      (-> (Listof T) T}
@tcoderm{          (U #false Natural)))])}
@tcoderm{(index-of '(a b) 'a)}
))
      (untyped-codeblock* (list
@tcoderm{(index-of '(a b) 'a)}
)))
    (yblank small-y-sep)
    (table2
      #:col-sep small-x-sep
      #:row-sep tiny-y-sep
      (list
        @bodyembf{Deep} @coderm{#false (not found)}
        @bodyembf{Shallow} @coderm{0 (found at position 0)}))
  )
  (void))

(define (sec:end)
  (pslide
    #:go heading-coord-l
    @headrm{In Conclusion}
    #:go hi-text-coord-m
    #:alt ( (transient-works 0) )
    #:alt ( (transient-works 1) )
    (transient-works 2)
    (yblank med-y-sep)
    (perf-bottom-line)
  )
  (pslide
    #:go center-coord
    (transient-gateway 0)
  )
  (pslide
    #:go center-coord
    @headrm{The End}
  )
  #;(pslide
    #:go heading-coord-l
    @headrm{In Conclusion}
    #:go hi-text-coord-m
    (transient-works 2)
    (yblank med-y-sep)
    (perf-bottom-line)
  )

  (void))

(define (sec:qa)
  (pslide
    #:go heading-coord-m
    @headrm{Optimizations}
    #:go hi-text-coord-m
    (ht-append
      med-x-sep
    (table2
      #:col-sep small-x-sep
      #:row-sep pico-y-sep
      (list
        ;; TODO shallow or transient?
        @bodyrmlo{Topic} @bodyrmlo{Ok for Transient?}
        @coderm{apply} @codeemrm{y}
        @coderm{box} @codeemrm{y}
        @coderm{dead-code} @codeembf{N}
        @coderm{extflonum} @codeemrm{y}
        @coderm{fixnum} @codeemrm{y}
        @coderm{float-complex} @codeemrm{y}
        @coderm{float} @codeemrm{y}
        ))
    (table2
      #:col-sep small-x-sep
      #:row-sep pico-y-sep
      (list
        @bodyrmlo{Topic} @bodyrmlo{Ok?}
        @coderm{list} @codeemrm{y}
        @coderm{number} @codeemrm{y}
        @coderm{pair} @codeembf{N}
        @coderm{sequence} @codeemrm{y}
        @coderm{string} @codeemrm{y}
        @coderm{struct} @codeemrm{y}
        @coderm{vector} @codeemrm{y})))
    (yblank tiny-y-sep)
    (hrule (w%->pixels 5/10))
    (yblank tiny-y-sep)
    @tcoderm{https://prl.ccs.neu.edu/blog/2020/01/15/the-typed-racket-optimizer-vs-transient}
  )
  (pslide
    #:go heading-coord-m
    @headrm{Example: Retic. and Dyn}
    #:go slide-text-coord-mid
    @bodyrmlo{Most of the local variables get the Dynamic type and skip blame-map updates}
    (yblank tiny-y-sep)
    (typed-codeblock* (list
@tcoderm{def permutations(iterable:List(int))->List(List(int)):}
@tcoderm{    pool = tuple(iterable)}
@tcoderm{    n = len(pool)}
@tcoderm{    r = n}
@tcoderm{    indices = list(range(n))}
@tcoderm{    cycles = list(range(n-r+1, n+1))[::-1]}
@tcoderm{    result = [ [pool[i] for i in indices[:r]] ]}
@tcoderm{    while n:}
@tcoderm{        for i in reversed(range(r)):}
@tcoderm{            cycles[i] -= 1}
@tcoderm{            if cycles[i] == 0:}
@tcoderm{                indices[i:] = indices[i+1:] + indices[i:i+1]}
@tcoderm{                cycles[i] = n - i}
@tcoderm{            else:}
@tcoderm{                ....}
;; @tcoderm{                j = cycles[i]}
;; @tcoderm{                indices[i], indices[-j] = indices[-j], indices[i]}
;; @tcoderm{                result.append([pool[i] for i in indices[:r]])}
;; @tcoderm{                break}
;; @tcoderm{        else:}
;; @tcoderm{            return result}
;; @tcoderm{    return result}
))
  )
  (pslide
    #:go heading-coord-m
    @headrm{Limitation}
    #:go hi-text-coord-m
    (word-append
      @bodyrmlo{Neither }
      @bodyembf{Guarded}
      @bodyrmlo{ nor }
      @bodyembf{Transient}
      @bodyrmlo{ TR allows occurrence types at a boundary})
    (yblank tiny-y-sep)
    (typed-codeblock* (list
@tcoderm{(require/typed racket/function}
@tcoderm{  (identity (-> Any Boolean : String)))}
@tcoderm{;; ^ Not permitted!}
@tcoderm{}
@tcoderm{(define x : Any 0)}
@tcoderm{}
@tcoderm{(define fake-str : String}
@tcoderm{  (if (identity x)}
@tcoderm{      (ann x String)}
@tcoderm{      (error 'unreachable)))}
@tcoderm{ }
@tcoderm{(string-length fake-str)}
))
  )
  (void))

(define (sec:unused)
  (pslide
    #:go heading-coord-m
    @headrm{Research Goal  =  Sound Gradual Typing}
    #:next
    #:alt (
      #:go (coord head-left slide-text-top 'lt)
      (word-append
        @bodyrmhi{Vision}
        @bodyrmlo{: in any codebase ....})
      #:go hi-text-coord-mid
      (yblank small-y-sep)
      (huge-program-example 0)
    )
    #:go (coord head-left slide-text-top 'lt)
    (word-append
      @bodyrmhi{Vision}
      @bodyrmlo{: in any codebase .... }
      @bodyrmhi{any components}
      @bodyrmlo{ can have }
      @bodyrmhi{reliable types})
    #:go hi-text-coord-mid
    (yblank small-y-sep)
    (huge-program-example 1)
  )
  (pslide
    #:go heading-coord-l
    @headrm{Example 1: Sound Types}
    #:go hi-text-coord-m
    #:alt ( (basic-example 'T #f))
    #:alt ( (basic-example 'T 'U))
    (basic-example 'T 'U #:lbl? #t)
  )
  (pslide
    #:go heading-coord-r
    @headrm{Example 2: Sound Types}
    #:next
    #:go hi-text-coord-m
    #:alt ( (bad-array-example) )
    (bad-array-example #:lbl? #t)
  )
  (pslide
    #:go heading-coord-m
    @headrm{Types for Debugging}
    #:next
    #:go hi-text-coord-mid
    (yblank small-y-sep)
    #:alt ( (huge-program-example 0) )
    #:alt ( (huge-program-example 2) )
    (huge-program-example 3)
  )
  (pslide
    #:go center-coord
    #:alt ( (the-problem 0) )
    (the-problem 1)
  )
  (pslide
    #:go heading-coord-l
    @headrm{Example: Run-Time Costs}
    #:go hi-text-coord-m
    #:alt ( (bad-array-example #:lbl? #t #:cost? 'ghost) )
    (bad-array-example #:lbl? #t #:cost? #t)
  )
  (pslide
    #:go heading-coord-l
    @headrm{Costs Really Add Up!}
    #:go hi-text-coord-l
    (memory-bg (vl-append tiny-y-sep (tr-app-head) (tr-app-body)))
    #:next
    #:go (coord 42/100 36/100 'lt)
    (bad-news @headrm{Q.   Is Sound Gradual Typing Dead?})
  )
  (pslide
    #:go (coord slide-text-left slide-text-top 'lt)
    (the-perf-problem)
    #:go (coord 1/2 slide-text-top 'ct)
    (bghost (the-perf-problem))
    (yblank small-y-sep)
    #:alt ( (perf-what-to-do 0) )
    #:alt ( (perf-what-to-do 1) )
    #:alt ( (perf-what-to-do 2) )
    #:alt ( (perf-what-to-do 3) )
    (perf-what-to-do 4)
  )
  (pslide
    #:go (coord 1/2 30/100 'ct)
    ;; TODO needs more color
    (scale
      (lc-append
        (word-append
          @bodyrmlo{"A way to } @bodyrmhi{reduce costs across the board})
        (word-append
          @bodyrmlo{without changing the surface language"}))
      12/10)
    (yblank tiny-y-sep)
    (arrow-bullet*
      @bodyrmlo{Same code, types, and type checker}
      (word-append
        @bodyrmlo{Different } @bodyrmhi{typed-untyped interactions}))
    #:go (coord 1/2 slide-text-top 'ct)
    (bghost (the-perf-problem))
    (yblank small-y-sep)
    (perf-what-to-do 5 #:only 3)
  )
  (pslide
    ;; TODO fix the little shuffle "How to Detect"
    ;; TODO time permitting, show the other program U-T-U to explain "weak"
    #:go center-coord
    ;; img/horizon
    #:go heading-coord-m
    (retic-head)
    (yblank pico-y-sep)
    (lc-append
      (word-append
        @bodyembf{Transient}
        @bodyrmlo{ = weakly-sound gradual types with } @bodyrmhi{no wrappers} @bodyrmlo{ and }
        @bodyrmhi{no traversals})
      (word-append
        @bodyemrm{Types enforce only top-level properties}))
    (yblank med-y-sep)
    #:alt ( (bad-array-example #:cost? 'ghost) )
    #:alt ( (bad-array-example #:retic? #t #:cost? #t) )
    #:next
    (ppict-do
      (retic-perf 1)
      #:go (coord 1/2 0 #:abs-y (- (* 1.6 tiny-y-sep)))
      (ppict-do
        @bodyrmlo{Performance is not bad!}
        #:go (coord 1 1/2 'lc #:abs-x tiny-x-sep)
        @coderm{[POPL'17]}))
  )
  (pslide
    #:go heading-coord-m
    (ppict-do
      @headrm{Challenges}
      #:go (coord 1/2 8/100 'ct)
      (scale (retic-vs-tr-pict 90/100) 8/10))
    #:next
    #:go center-coord
    (roadblock-pict
      (word-append
        @bodyrmhi{Transient blame is } @bodyrmhi{very expensive}))
  )
  (pslide
    #:go heading-coord-l
    @headrm{Blame: The Idea}
    #:go heading-coord-m
    (bghost @headrm{B})
    (yblank small-y-sep)
    #:alt ( (blame-illustration 0) )
    #:alt ( (blame-illustration 2) )
    (blame-illustration 3)
    (yblank small-y-sep)
    (lc-append
      (word-append
        @bodyrmlo{When a } @bodyemty{typed} @bodyrmlo{/}
        @bodyemun{untyped} @bodyrmlo{ interaction goes wrong,})
      (word-append
        @bodyembl{blame} @bodyrmlo{ shows where to begin debugging}))
    #:next
    (yblank small-y-sep)
    #:alt ( (blame-how 0) )
    #:alt ( (blame-how 1) )
    (blame-how 2)
  )

  (void))

;; --- 

(define bg-orig (current-slide-assembler))
(define bg-cs.brown.edu (slide-assembler/background bg-orig make-bg))
(define title-cs.brown.edu (slide-assembler/background bg-orig make-titlebg))

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
                 (pplay-steps 7))
    (sec:unsorted)

    (sec:intro)
    (sec:rp)
    (sec:noblame-perf)
    (sec:expr)
    (sec:end)

    (pslide)
    (sec:qa)
    (pslide)

    (void))
  (void))

;; -----------------------------------------------------------------------------

(module+ main
  (do-show))

;; =============================================================================

(module+ raco-pict (provide raco-pict)
         ;;(define client-w 984) (define client-h 728)
         (define client-w 1320) (define client-h 726)
         (define raco-pict
  (ppict-do
    (make-bg client-w client-h)
    #;(make-titlebg client-w client-h)



  )))
