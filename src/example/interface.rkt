#lang typed/racket

(require (only-in typed/racket/draw Font% Bitmap%))

(require/typed/provide
  "untyped.rkt"
  (text-icon (-> String (Instance Font%)
                 (Instance Bitmap%))))
