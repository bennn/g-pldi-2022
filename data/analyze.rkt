#lang racket/base

(provide
  benchmark-name->performance-info
  get-mixed-path-table
  render-mixed-path-table
  find-lowest-3dpath-D
  find-lowest-3dpath-D*
  get-3d-table
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
  with-cache
  (only-in "../main.rkt" glob-first stransient default-rkt-version transient-rkt-version bm)
  (only-in math/number-theory factorial)
  (only-in math/statistics mean)
  (only-in scribble/base bold centered hyperlink tabular hspace tt linebreak)
  (only-in gtp-util rnd pct string->value time-string->cpu-time natural->bitstring))

;; -----------------------------------------------------------------------------

(define-runtime-path HERE ".")
(define cache-dir (build-path HERE ".." "with-cache"))
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

(define (find-lowest-3dpath-D bm-name)
  (string->number (rnd
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () bm-name))]
                 [*with-cache-fasl?* #f])
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
                   [*with-cache-fasl?* #f])
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
        "worst before"
        "worst after"
        "   improvement"))

(define (render-mixed-worst-table row*)
  ;; TODO abstraction
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(bottom-border 1)
      #:column-properties '(left right)
      (list* MIXED-WORST-TITLE
             (map cdr row*)))))

(define (get-mixed-worst-table name*)
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () name*))]
                 [*with-cache-fasl?* #f])
    (with-cache (cachefile "mixed-worst-table.rktd")
      (λ ()
        (for/list ([name (in-list name*)])
          (make-mixed-worst-row name
                          (benchmark-name->performance-info name stransient)
                          (benchmark-name->performance-info name default-rkt-version)))))))

(define (make-mixed-worst-row name pi-shallow pi-deep)
  (define s-max (max-overhead pi-shallow))
  (define d-max (max-overhead pi-deep))
  (define worst-before (max s-max d-max))
  (define worst-after (min s-max d-max))
  (define x-improved (/ worst-before worst-after))
  (list name
        (bm name)
        (string-append (rnd worst-before) "x")
        (string-append (rnd worst-after) "x")
        (let ((v (exact-floor x-improved)))
          (if (= v 1)
            "<2x"
            (format "~ax" v)))))

;; ---

(define MIXED-PATH-TITLE
  (list "Benchmark"
        "Deep %"
        "Shallow %"
        "D. or S. %"))

(define (render-mixed-path-table row*)
  ;; TODO abstraction
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(bottom-border 1)
      #:column-properties '(left right)
      (list* MIXED-PATH-TITLE
             (map cdr row*)))))

(define (get-mixed-path-table D name*)
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () (cons D name*)))]
                 [*with-cache-fasl?* #f])
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

(define (get-3d-table bm-name*)
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () bm-name*))]
                 [*with-cache-fasl?* #f])
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

;; -----------------------------------------------------------------------------

(module+ main
  (quick-table (glob (build-path "nsa-2020-12-30" "*.out"))))
