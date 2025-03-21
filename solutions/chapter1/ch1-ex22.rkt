#lang racket/base

(provide prime?
         search-for-primes)

(require akari-sicp/solutions/chapter1/ch1-ex21)

(define runtime current-milliseconds)

(define (prime? n)
  (= n (smallest-divisor n)))

(define (report-prime elapsed-time)
  (display  " *** ")
  (display elapsed-time))

(define (start-prime-test prime-test n start-time)
  (when (prime-test n)
    (report-prime (- (runtime) start-time))))

(define (timed-prime-test prime-test n)
  (newline)
  (display n)
  (start-prime-test prime-test n (runtime)))

(define (search-for-primes prime-test start n)
  (cond [(= n 0) (void)]
        [(even? start) (search-for-primes prime-test (+ start 1) n)]
        [(prime? start)
         (timed-prime-test prime-test start)
        (search-for-primes prime-test (+ start 2) (- n 1))]
        [else (search-for-primes prime-test (+ start 2) n)]))

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
1000000000039 *** 3
1000000000061 *** 3
1000000000063 *** 3

10000000000037 *** 10
10000000000051 *** 11
10000000000099 *** 11

100000000000031 *** 32
100000000000067 *** 32
100000000000097 *** 32

1000000000000037 *** 100
1000000000000091 *** 100
1000000000000159 *** 99
|#