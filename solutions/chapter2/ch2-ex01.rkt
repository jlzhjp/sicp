#lang racket/base

(require (only-in racket/math sgn)
         (only-in akari-sicp/solutions/chapter1/ch1-ex20 gcd))

(define (make-rat n d)
  (let* ([g (gcd n d)]
         [n (/ n g)]
         [d (/ d g)]
         [s (sgn d)])

    (if (< s 0)
        (cons (- n) (- d))
        (cons n d))))

(module+ test
  (require rackunit)

  (check-equal? (make-rat 2 4) (cons 1 2))
  (check-equal? (make-rat -2 -4) (cons 1 2))
  (check-equal? (make-rat -2 4) (cons -1 2))
  (check-equal? (make-rat 2 -4) (cons -1 2)))