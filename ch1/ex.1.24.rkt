#lang racket/base

(provide expmod)

(require sicp-lib)
(require "ex.1.22.rkt")

(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m)]
        [else
         ; About why remainder is not needed for base:
         ; https://stackoverflow.com/questions/64672675/
         (remainder (* base (expmod base (- exp 1) m))
                    m)]))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (min 4294967087 (- n 1))))))

(define test-times 1000)

(define (fast-prime? n times)
  (cond [(= times 0) #t]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else #f]))

(define (fast-prime-fixed-times? n)
  (fast-prime? n test-times))

(module+ test
  (require rackunit)

  (check-true (fast-prime-fixed-times? 2))
  (check-true (fast-prime-fixed-times? 3))
  (check-false (fast-prime-fixed-times? 4))
  (check-true (fast-prime-fixed-times? 5))
  (check-false (fast-prime-fixed-times? 6))
  (check-true (fast-prime-fixed-times? 7))
  (check-false (fast-prime-fixed-times? 8))
  (check-false (fast-prime-fixed-times? 9))
  (check-false (fast-prime-fixed-times? 10)))

(module+ main
  (search-for-primes fast-prime-fixed-times? 1000000000000 3)
  (newline)
  (search-for-primes fast-prime-fixed-times? 10000000000000 3)
  (newline)
  (search-for-primes fast-prime-fixed-times? 100000000000000 3)
  (newline)
  (search-for-primes fast-prime-fixed-times? 1000000000000000 3))