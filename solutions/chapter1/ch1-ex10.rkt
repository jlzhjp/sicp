#lang racket/base

(define (A x y)
  (cond [(= y 0) 0]
        [(= x 0) (* 2 y)]
        [(= y 1) 2]
        [else (A (- x 1)
                 (A x (- y 1)))]))

; (displayln (A 1 10))
; (displayln (A 2 4))
; (displayln (A 3 3))

(module+ test
  (require rackunit)

  (check-= (A 1 10) 1024 0)
  (check-= (A 2 4) 65536 0)
  (check-= (A 3 3) 65536 0))

(module+ test
  (define (f n) (A 0 n))
  (define (g n) (A 1 n))
  (define (h n) (A 2 n))

  (test-case
   "test f"
   (check-= (f 0) 0 0)
   (check-= (f 1) 2 0)
   (check-= (f 2) 4 0))

  (test-case
   "test g"
   (check-= (g 0) 0 0)
   (check-= (g 1) 2 0)
   (check-= (g 2) 4 0)
   (check-= (g 3) 8 0)
   (check-= (g 4) 16 0))

  (test-case
   "test h"
   (check-= (h 0) 0 0)
   (check-= (h 1) 2 0)
   (check-= (h 2) 4 0)
   (check-= (h 3) 16 0)
   (check-= (h 4) 65536 0)))