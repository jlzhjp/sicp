#lang racket/base

(require "ex.1.21.rkt")

(define runtime current-milliseconds)

(define (prime? n)
  (= n (smallest-divisor n)))

(define (report-prime elapsed-time)
  (display  " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (when (prime? n)
    (report-prime (- (runtime) start-time))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes start n)
  (cond [(= n 0) (void)]
        [(even? start) (search-for-primes (+ start 1) n)]
        [(prime? start)
         (timed-prime-test start)
        (search-for-primes (+ start 2) (- n 1))]
        [else (search-for-primes (+ start 2) n)]))

; (search-for-primes 1000000000000 3)
; (newline)
; (search-for-primes 10000000000000 3)
; (newline)
; (search-for-primes 100000000000000 3)

#|
1000000000039 *** 4
1000000000061 *** 4
1000000000063 *** 4

10000000000037 *** 14
10000000000051 *** 13
10000000000099 *** 13

100000000000031 *** 38
100000000000067 *** 40
100000000000097 *** 39
|#