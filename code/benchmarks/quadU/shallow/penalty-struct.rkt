#lang typed/racket/base #:transient

;; For wrap.rkt

(provide (struct-out $penalty))

;; =============================================================================

(struct $penalty
  ([hyphens : Natural]
   [width   : Float]
) #:transparent)

