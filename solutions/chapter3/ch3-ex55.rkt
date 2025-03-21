#lang racket/base

(require akari-sicp/lib/stream
         "ch3-ex50.rkt" ; stream-map
         "ch3-ex54.rkt") ; integers

; Exercise 3.55
; Define a procedure partial-sum that takes as argument a stream S
; and return the stream whose elements are S0, S0 + S1, S0 + S1 + S2, ...
; For example, (partial-sum integers) should be the stream 1, 3, 6, 10, 15, ...

;       S1    S2       S3
;       S0    S0+S1    S0+S1+S2
; s: S0 S0+S1 S0+S1+S2 S0+S1+S2+S3
;

(define (partial-sum s)
  (letrec ([rs
            (cons-stream (stream-car s)
                         (stream-map + rs (stream-cdr s)))])
    rs))

;; Tests for partial-sum function
(module+ test
  (require rackunit/text-ui akari-sicp/lib/testing)

  ;; Define test suite
  (define-test-suite partial-sum-tests
    (test-case
     "Testing partial-sum with integers:"
     (check-stream-prefix-= (partial-sum integers)
                            (list 1 3 6 10 15 21)))
    (test-case
     "Testing partial-sum with squares:")
    (let ([squares (stream-map (lambda (x) (* x x)) integers)])
      ;; Expected partial sums of squares: 1, 5, 14, 30, 55, 91
      (check-stream-prefix-= (partial-sum squares)
                             (list 1 5 14 30 55 91))))

  (run-tests partial-sum-tests))