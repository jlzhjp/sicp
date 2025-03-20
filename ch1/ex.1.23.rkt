#lang racket/base

(require sicp-lib)
(require "ex.1.22.rkt")

(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (define (next test-divisor)
    (if (even? test-divisor)
        (+ test-divisor 1)
        (+ test-divisor 2)))
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (next test-divisor))]))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(module+ test)

(module+ main
  (search-for-primes prime? 1000000000000 3)
  (newline)
  (search-for-primes prime? 10000000000000 3)
  (newline)
  (search-for-primes prime? 100000000000000 3)
  (newline)
  (search-for-primes prime? 1000000000000000 3))

#|
1000000000039 *** 2
1000000000061 *** 2
1000000000063 *** 3

10000000000037 *** 7
10000000000051 *** 7
10000000000099 *** 7

100000000000031 *** 23
100000000000067 *** 23
100000000000097 *** 23

1000000000000037 *** 72
1000000000000091 *** 72
1000000000000159 *** 72
|#