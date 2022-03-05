#lang racket/base

(provide
  benchmark-name->performance-info
  get-mixed-path-table
  render-mixed-path-table
  find-lowest-3dpath-D
  find-lowest-3dpath-D*
  get-3d-table
  render-3d-table-x
  render-3d-table-y
  get-mixed-worst-table
  render-mixed-worst-table)

(require
  file/glob
  racket/format
  racket/math
  racket/list
  racket/path
  racket/runtime-path
  racket/sequence
  gtp-plot/configuration-info
  gtp-plot/performance-info
  gtp-plot/typed-racket-info
  "../src/with-cache/with-cache.rkt"
  (only-in "../g-pldi-2022/main.rkt" glob-first stransient default-rkt-version transient-rkt-version bm)
  (only-in math/number-theory factorial)
  (only-in math/statistics mean median)
  (only-in scribble/base bold centered hyperlink tabular hspace tt linebreak)
  (only-in gtp-util rnd pct string->value time-string->cpu-time natural->bitstring))

(module+ test (require rackunit))

;; -----------------------------------------------------------------------------

(define-runtime-path HERE ".")
(define cache-dir (build-path HERE ".." "src" "with-cache"))
(define data-dir HERE)
(define data-3d-dir* (list (build-path HERE "nsa-2020-11-04") (build-path HERE "nsa-2020-12-30")))

(define (quick-make-3d-table fn*)
  (for/list ((fn (in-list fn*)))
    (define pi (filename->performance-info3d fn))
    (define bm (filename->benchmark-name fn))
    (define total-configs (performance-info->num-configurations pi))
    (define num-good
      (with-input-from-file
        (filename-3d fn)
        (lambda ()
          (void (read-line))
          (for/sum ((cfg (in-lines))
                    #:when (has-mix? (car (string->value cfg))))
            1))))
    (list bm (rnd (pct num-good total-configs)))))

(define (benchmark-name->performance-info bm-name version #:full-name? [full-name? #f])
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

(define (filename->performance-info3d orig-filename)
  (define best-cfg*
    (with-input-from-file
      orig-filename
      (lambda ()
        (void (read-line)) ;; ignore lang
        (define (read-cfg) (string->value (read-line)))
        (define untyped-cfg (read-cfg))
        (define num-units (string-length (car untyped-cfg)))
        (cons
          untyped-cfg
          (for/list ((i (in-range (- (expt 2 num-units) 1))))
            (define cfg0 (read-cfg))
            (define num-types (for/sum ((c (in-string (car cfg0))) #:unless (eq? #\0 c)) 1))
            (define cfg* (for/list ((j (in-range (- (expt 2 num-types) 1)))) (read-cfg)))
            (car (sort (cons cfg0 cfg*) <=2 #:key cfg->simple-time+mix #:cache-keys? #true)))))))
  (define best-filename (filename-3d orig-filename))
  (define bm-name (filename->benchmark-name orig-filename))
  (void
    (with-output-to-file
      best-filename
      #:exists 'replace
      (lambda ()
        (displayln "#lang gtp-measure/output/typed-untyped")
        (for ((ln (in-list best-cfg*)))
          (writeln ln)))))
  (make-typed-racket-info best-filename #:name (string->symbol (format "~a-3d" bm-name))))

(define (<=2 p0 p1)
  (or (<= (car p0) (car p1))
      (and (= (car p0) (car p1))
           (<= (cdr p0) (cdr p1)))))

(define (cfg->simple-time val)
  (string->number (rnd (mean (map time-string->cpu-time (cadr val))))))

(define (cfg->simple-time+mix val)
  (cons (cfg->simple-time val) (has-mix? (car val))))

(define (filename-3d orig-filename)
  (path-add-extension (~a orig-filename) ".3d"))

(define (filename-3d+ bm-name)
  (define orig-filename
    (or
      (for/or ((dir-3d (in-list data-3d-dir*)))
        (let ((m* (append
                    (glob (build-path dir-3d (format "~a-*.rktd" bm-name)))
                    (glob (build-path dir-3d (format "*-~a.out" bm-name))))))
          (if (or (null? m*) (not (null? (cdr m*))))
            #f
            (car m*))))
      (raise-arguments-error 'filename-3d+ "no data found" "benchmark" bm-name "data-dirs" data-3d-dir*)))
  (define best-filename (path-add-extension orig-filename ".3d"))
  (values orig-filename best-filename))

(define (filename->benchmark-name fn)
  (let-values (((base name mbd?) (split-path fn)))
    (cadr (regexp-match #rx"^[0-9]+-([^.]+)\\.out$" (path->string name)))))

(define (has-mix? str)
  (define c* (string->list str))
  (for/and ((xxx (in-list '(#\0 #\1 #\2))))
    (memq xxx c*)))

(define (all-paths pi)
  (define num-units (performance-info->num-units pi))
  (all-paths-from (natural->bitstring 0 #:bits num-units)))

(define (all-paths-from str)
  (sequence-map
    permutation->path
    (in-permutations (range (string-length str)))))

(define (permutation->path index*)
  (define L (length index*))
  ;; Create the path in reverse order
  (for/fold ([acc (list (bitstring-init L #:hi? #t))])
            ([i (in-list index*)])
    (cons (bitstring-flip (car acc) i) acc)))

(define (bitstring-init n #:hi? [hi? #f])
  (make-string n (if hi? #\1 #\0)))

(define (bitstring-flip str i)
  (define new (if (equal? #\0 (string-ref str i)) "1" "0"))
  (string-append (substring str 0 i)
                 new
                 (substring str (add1 i) (string-length str))))

(define (2d-mask str)
  (apply
    string
    (for/list ((c (in-string str)))
      (if (eq? c #\0)
        c
        #\1))))

(define (find-lowest-3dpath-D bm-name)
  (string->number (rnd
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () bm-name))]
                 #;[*with-cache-fasl?* #f])
    (with-cache (cachefile (format "mixed-bestpath-~a.rktd" bm-name))
      (λ ()
        (define pi (benchmark-name->performance-info3d bm-name))
        (define total-paths (factorial (performance-info->num-units pi)))
        (or
          (for/or ((pre-d (in-range 10)))
            (define D (+ 1 (/ pre-d 10)))
            (and (= total-paths (count-deliv-path D pi))
                 D))
          (raise-arguments-error 'both "cannot find D for 100% paths" "bm" bm-name))))))))

(define (find-lowest-3dpath-D* bm-name*)
  (map (lambda (nv) (cons (car nv) (if (cdr nv) (string->number (rnd (cdr nv))) #f)))
    (parameterize ([*current-cache-directory* cache-dir]
                   [*current-cache-keys* (list (λ () bm-name*))]
                   #;[*with-cache-fasl?* #f])
      (with-cache (cachefile "mixed-bestpath.rktd")
        (λ ()
          (for/list ((bm-name (in-list bm-name*)))
            (define pi (benchmark-name->performance-info3d bm-name))
            (define total-paths (factorial (performance-info->num-units pi)))
            (cons
              bm-name
              (for*/or ((fst (in-range 1 5))
                        (pre-d (in-range 10)))
                (define D (+ fst (/ pre-d 10)))
                (and (= total-paths (count-deliv-path D pi))
                     D)))))))))

(define (benchmark-name->performance-info3d bm-name)
  (define-values [orig-filename best-filename] (filename-3d+ bm-name))
  (unless (file-exists? best-filename)
    (let ((best-cfg*
           (with-input-from-file
             orig-filename
             (lambda ()
               (void (read-line)) ;; ignore lang
               (define (read-cfg) (string->value (read-line)))
               (define untyped-cfg (read-cfg))
               (define num-units (string-length (car untyped-cfg)))
               (cons
                 untyped-cfg
                 (for/list ((i (in-range (- (expt 2 num-units) 1))))
                   (define cfg0 (read-cfg))
                   (define num-types (for/sum ((c (in-string (car cfg0))) #:unless (eq? #\0 c)) 1))
                   (define cfg* (for/list ((j (in-range (- (expt 2 num-types) 1)))) (read-cfg)))
                   (car (sort (cons cfg0 cfg*) <=2 #:key cfg->simple-time+mix #:cache-keys? #true))))))))
      (with-output-to-file
        best-filename
        (lambda ()
          (displayln "#lang gtp-measure/output/typed-untyped")
          (for ((ln (in-list best-cfg*)))
            (writeln ln))))))
  (make-typed-racket-info best-filename #:name (string->symbol (format "~a-3d" bm-name))))

;; ---

(define (quick-table fn*)
  (define DDD (quick-make-3d-table fn*))
  (for ((d-row (in-list DDD)))
    (printf " ~a% of ~a configs~n" (cadr d-row) (car d-row)))
  (void))

(define MIXED-WORST-TITLE
  (list "Benchmark"
        "Worst Deep"
        "Worst Shallow"
        "Worst D∥S"))

(define (render-mixed-worst-table row*)
  ;; TODO abstraction
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(bottom-border 1)
      #:column-properties '(left right)
      (list* MIXED-WORST-TITLE
             (map cleanup-mixed-row row*)))))

(define (cleanup-mixed-row rr)
  (let* ((rr (cdr rr))
         (fst (car rr))
         (rst* (map sig2 (cdr rr)))
         (d (first rst*))
         (s (second rst*))
         (ds (third rst*)))
    (list (car rr) d s (bold-if-winning ds d s))))

(define (get-mixed-worst-table name*)
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () name*))]
                 #;[*with-cache-fasl?* #f])
    (with-cache (cachefile "mixed-worst-table.rktd")
      (λ ()
        (for/list ([name (in-list name*)])
          (make-mixed-worst-row name
                          (benchmark-name->performance-info name stransient)
                          (benchmark-name->performance-info name default-rkt-version)))))))

(define (make-mixed-worst-row name pi-shallow pi-deep)
  (define s-max (max-overhead pi-shallow))
  (define d-max (max-overhead pi-deep))
  (define worst-after
    (for/fold ((acc 0))
              ((s-cfg (in-configurations pi-shallow))
               (d-cfg (in-configurations pi-deep)))
      (unless (equal? (configuration-info->id s-cfg)
                      (configuration-info->id d-cfg))
        (raise-argument-error 'make-mixed-worst-row
                              "out-of-sync configurations"
                              "shallow" s-cfg
                              "deep" d-cfg
                              "s-pi" pi-shallow
                              "d-pi" pi-deep))
      (max acc
           (min (overhead pi-shallow (configuration-info->mean-runtime s-cfg))
                (overhead pi-deep (configuration-info->mean-runtime d-cfg))))))
  (list name
        (bm name)
        (string-append (rnd d-max) "x")
        (string-append (rnd s-max) "x")
        (string-append (rnd worst-after) "x")))

;; ---

(define MIXED-PATH-TITLE
  (list "Benchmark"
        "Deep paths"
        "Shallow paths"
        "D∥S paths"))

(define (render-mixed-path-table row*)
  ;; TODO abstraction
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(bottom-border 1)
      #:column-properties '(left right)
      (list* MIXED-PATH-TITLE
             (map (lambda (r)
                    (cons (cadr r)
                          (cleanup-path-row (cddr r))))
                  row*)))))

(define (cleanup-path-row str*)
  (define deep-str (car str*))
  (define shallow-str (cadr str*))
  (define ds-str (caddr str*))
  (define ds+
    (let ((n0 (string->number deep-str))
          (n1 (string->number shallow-str))
          (n2 (string->number ds-str)))
      (if (< (max n0 n1) n2)
        (bold (add% ds-str))
        (add% ds-str))))
  (list (add% deep-str) (add% shallow-str) ds+))

(define (add% str)
  (string-append str "%"))

(define (get-mixed-path-table D name*)
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () (cons D name*)))]
                 #;[*with-cache-fasl?* #f])
    (with-cache (cachefile "mixed-path-table.rktd")
      (λ ()
        (for/list ([name (in-list name*)])
          (make-mixed-path-row D name
                          (benchmark-name->performance-info name stransient)
                          (benchmark-name->performance-info name default-rkt-version)))))))

(define (make-mixed-path-row D name pi-shallow pi-deep)
  (define total-paths (factorial (performance-info->num-units pi-deep)))
  (define (fmt n0 n1)
    (~a (exact-round (pct n0 n1))))
  (list name
        (bm name)
        (fmt (count-deliv-path D pi-deep) total-paths)
        (fmt (count-deliv-path D pi-shallow) total-paths)
        (fmt (count-deliv-path D pi-shallow pi-deep) total-paths)))

;; D = 2 data
;;    ((sieve)
;;     (list name (bm name) "0" "0" "0"))
;;    ((forth)
;;     (list name (bm name) "0" "0" "17"))
;;    ((fsm)
;;     (list name (bm name) "33" "0" "100"))
;;    ((fsmoo)
;;     (list name (bm name) "0" "0" "50"))
;;    ((mbta)
;;     (list name (bm name) "100" "100" "100"))
;;    ((morsecode)
;;     (list name (bm name) "100" "0" "100"))
;;    ((zombie)
;;     (list name (bm name) "0" "0" "0"))
;;    ((dungeon)
;;     (list name (bm name) "0" "0" "0"))
;;    ((jpeg)
;;     (list name (bm name) "0" "100" "100"))
;;    ((zordoz)
;;     (list name (bm name) "50" "0" "67"))
;;    ((lnm)
;;     (list name (bm name) "100" "100" "100"))
;;    ((suffixtree)
;;     (list name (bm name) "0" "0" "0"))
;;    ((kcfa)
;;     (list name (bm name) "0" "100" "100"))
;;    ((snake)
;;     (list name (bm name) "0" "0" "0"))
;;    ((take5)
;;     (list name (bm name) "0" "0" "16"))

;; D = 3
;;   ((sieve)
;;    (list name (bm name) "0" "0" "100"))
;;   ((forth)
;;    (list name (bm name) "0" "0" "50"))
;;   ((fsm)
;;    (list name (bm name) "100" "100" "100"))
;;   ((fsmoo)
;;    (list name (bm name) "0" "0" "50"))
;;   ((mbta)
;;    (list name (bm name) "100" "100" "100"))
;;   ((morsecode)
;;    (list name (bm name) "100" "100" "100"))
;;   ((zombie)
;;    (list name (bm name) "0" "0" "50"))
;;   ((dungeon)
;;    (list name (bm name) "0" "0" "67"))
;;   ((jpeg)
;;    (list name (bm name) "0" "100" "100"))
;;   ((zordoz)
;;    (list name (bm name) "100" "100" "100"))
;;   ((lnm)
;;    (list name (bm name) "100" "100" "100"))
;;   ((suffixtree)
;;    (list name (bm name) "0" "0" "12"))
;;   ((kcfa)
;;    (list name (bm name) "33" "100" "100"))
;;   ((snake)
;;    (list name (bm name) "0" "0" "0"))
;;   ((take5)
;;    (list name (bm name) "0" "100" "100"))
;;    (else
;;      (raise-argument-error 'make-mixed-path-row "simple-bm-name?" name))))

(define (count-deliv-path D . pi*)
  (when (null? pi*)
    (raise-argument-error 'count-deliv-path "(non-empty-listof performance-info?)" pi*))
  (for*/sum ((cfg-id* (all-paths (car pi*)))
             #:when (andmap (lambda (cfg-id)
                              (<= (apply min (map (lambda (pi) (overhead+ pi cfg-id)) pi*)) D))
                            cfg-id*))
      1))

(define (overhead+ pi cfg-id)
  (/ (performance-info->runtime pi cfg-id)
     (performance-info->untyped-runtime pi)))

(define (performance-info->runtime pi cfg-id)
  (or
    (for/first ((cfg (in-configurations pi))
                #:when (equal? cfg-id (configuration-info->id cfg)))
      (configuration-info->mean-runtime cfg))
    (raise-arguments-error 'performance-info->runtime "cfg not found" "pi" pi "cfg" cfg-id)))

(define DDD-TITLE-X '("Benchmark" "Best with D+S"))
(define DDD-TITLE-Y '("Benchmark" "Best w/ D+S"))

(define (render-3d-table-x ddd)
  (render-3d-table ddd 'x))

(define (render-3d-table-y ddd)
  (render-3d-table ddd 'y))

(define (render-3d-table ddd how)
  (centered
    (case how
      ((x)
       (tabular
         #:sep (hspace 8)
         #:style 'block
         #:row-properties '(1)
         #:column-properties '(left)
         (let-values (((l* r*) (split-at ddd (quotient (length ddd) 2))))
           (list (list (ddd-table-x l*) (ddd-table-x r*))))))
      ((y)
       (tabular
         #:sep (hspace 2)
         #:style 'block
         #:row-properties '(1)
         #:column-properties '(left)
         (let-values (((l* r*) (split-at ddd (quotient (length ddd) 2))))
           (list (list (ddd-table-y l*) (ddd-table-y r*)))))
       #;(ddd-table ddd))
      (else
        (raise-argument-error 'render-3d-table "(or/c 'x 'y)" 1 ddd how)))))

(define (ddd-table-x ddd)
  (ddd-table ddd DDD-TITLE-X))

(define (ddd-table-y ddd)
  (ddd-table ddd DDD-TITLE-Y))

(define (ddd-table ddd title)
  (tabular
    #:sep (hspace 2)
    #:style 'block
    #:row-properties '(bottom-border 1)
    #:column-properties '(left right)
    (list* title
           (for/list ((r (in-list ddd)))
             (list (bm (car r))
                   (format "~a%" (exact-round (string->number (cadr r)))))))))

(define (get-3d-table bm-name*)
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () bm-name*))]
                 #;[*with-cache-fasl?* #f])
    (with-cache (cachefile "mixed-3d-best.rktd")
      (λ ()
        (for/list ((bm (in-list bm-name*)))
          (define pi (benchmark-name->performance-info3d bm))
          (define total-configs (performance-info->num-configurations pi))
          (define-values [_orig best-filename] (filename-3d+ bm))
          (define num-good
            (with-input-from-file
              best-filename
              (lambda ()
                (void (read-line))
                (for/sum ((cfg (in-lines))
                          #:when (has-mix? (car (string->value cfg))))
                  1))))
          (list bm (rnd (pct num-good total-configs))))))))

(define (sig2 pre-num)
  ;; cut to 2 significant figures
  (define num (overhead-str->number pre-num))
  (if num
    (let ((short-num
           (cond
             [(< num 10)
              num]
             [(< num 100)
              (exact-round num)]
             [else
               (let ((k (expt 10 (sub1 (order-of-magnitude num)))))
                 (* k (exact-round (/ num k))))])))
      (format "~ax" short-num))
    pre-num))

(module+ test
  (test-case "sig2"
    (check-equal? (sig2 "3.14x") "3.14x")
    (check-equal? (sig2 "31x") "31x")
    (check-equal? (sig2 "31.4x") "31x")
    (check-equal? (sig2 "31.8x") "32x")
    (check-equal? (sig2 "314x") "310x")
    (check-equal? (sig2 "31456x") "31000x")))

(define (bold-if-winning after before0 before1)
  (define n2 (overhead-str->number after))
  (define n0 (overhead-str->number before0))
  (define n1 (overhead-str->number before1))
  (if (and n0 n1 n2 (< n2 n0) (< n2 n1))
    (bold after)
    after))

(define (overhead-str->number str)
  (define L (string-length str))
  (string->number
    (if (and (< 0 L)
             (eq? #\x (string-ref str (sub1 L))))
      (substring str 0 (sub1 L))
      str)))

(define (biggest-3d-gap* bm*)
  (for ((bm-name (in-list bm*)))
    (define gap
      (biggest-3d-gap (benchmark-name->performance-info bm-name stransient)
                      (benchmark-name->performance-info bm-name default-rkt-version)
                      (benchmark-name->performance-info3d bm-name)))
    (printf "~a : ~a~n" bm-name (map (lambda (x) (if (real? x) (rnd x) x)) gap))
    (void)))

(define (biggest-3d-gap pi-shallow pi-deep pi-3d)
  (for/fold ((acc #f))
            ((c-shallow (in-configurations pi-shallow))
             (c-deep (in-configurations pi-deep))
             (c-3d (in-configurations pi-3d)))
    (unless (string=?
              (configuration-info->id c-shallow)
              (configuration-info->id c-deep)
              (2d-mask (configuration-info->id c-3d)))
      (raise-argument-error 'biggest-3d-gap
                            "out-of-sync configurations"
                            "shallow" c-shallow
                            "deep" c-deep
                            "3d" c-3d))
    (define o-shallow (overhead pi-shallow (configuration-info->mean-runtime c-shallow)))
    (define o-deep (overhead pi-deep (configuration-info->mean-runtime c-deep)))
    (define o-3d (overhead pi-3d (configuration-info->mean-runtime c-3d)))
    (define best-2d (min o-shallow o-deep))
    (define gap (- best-2d o-3d))
    (if (or (not acc) (> gap (car acc)))
      (list gap '3d o-3d '2d best-2d)
      acc)))

;; -----------------------------------------------------------------------------

(module+ main
  (quick-table (glob (build-path "nsa-2020-11-04" "*.out")))
  (newline)
  (quick-table (glob (build-path "nsa-2020-12-30" "*.out")))

  #;(biggest-3d-gap* '(forth fsm fsmoo mbta morsecode zombie dungeon
                     jpeg zordoz lnm suffixtree kcfa snake take5
                     acquire tetris ))
  #;(let ((worst* '(2.97 5.43 1.91 4.25 1.71 1.3 31 3.16 1.56 2.58 1.17 5.8 1.24
                  7.61 2.97 1.42 5.44 4.2 1.51 7.23 7.45)))
    (printf "med ~a avg ~a~n"
            (median < worst*)
            (mean worst*)))

  (void))
