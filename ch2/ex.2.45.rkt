#lang racket/base

(provide right-split
         up-split)

(require "ex.2.51.rkt")

(define (split d1 d2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ([smaller ((split d1 d2) painter (- n 1))])
          (d1 painter (d2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(module+ test)