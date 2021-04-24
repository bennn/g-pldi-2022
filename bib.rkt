#lang racket/base

(provide ~cite citet)

(require
  (only-in "main.rkt" exact))

(define (~cite . txt*)
  (exact (list "~\\citep{" txt* "}")))

(define (citet . txt*)
  (exact (list "\\citet{" txt* "}")))

