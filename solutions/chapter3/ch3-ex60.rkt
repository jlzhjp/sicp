#lang racket/base

(require akari-sicp/lib/stream
         (only-in "ch3-ex53.rkt" add-streams)
         (only-in "ch3-ex56.rkt" scale-stream)
         (only-in "ch3-ex59.rkt" sine-series cosine-series))

; Exercise 3.60: With power series represented as streams
; of coefficients as in Exercise 3.59, adding series is
; implemented by add-streams. Complete the definition of the
; following procedure for multiplying series:

(define (mul-series s1 s2)
  (let ([a0 (stream-car s1)]
        [b0 (stream-car s2)]
        [s1. (stream-cdr s1)]
        [s2. (stream-cdr s2)])
    (cons-stream (* a0 b0)
                 (add-streams (scale-stream s2. a0)
                              (scale-stream s1. b0)
                              (cons-stream 0 (mul-series s1. s2.))))))

(module+ main
  (displayln
   (collect-stream
    (add-streams (mul-series sine-series sine-series)
                 (mul-series cosine-series cosine-series))
    10)))

(module+ test
  (require racket/list
           akari-sicp/lib/testing)

  ; Test that sin²(x) + cos²(x) = 1
  (check-stream-prefix-=
   (add-streams (mul-series sine-series sine-series)
                (mul-series cosine-series cosine-series))
   (cons 1 (make-list 15 0))))