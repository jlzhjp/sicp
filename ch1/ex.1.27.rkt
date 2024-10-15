#lang racket/base

(require racket/list)
(require (only-in "ex.1.24.rkt" expmod))

(define carmichael-numbers '(561 1105 1729 2465 2821 6601))

(define (full-fermat-test n)
  (andmap (lambda (a) (= (expmod a n n) a))
          (range 1 n)))

(module+ test
  (require rackunit)

  (check-true (full-fermat-test 2))
  (check-true (full-fermat-test 97))
  (check-false (full-fermat-test 16))
  (for ([cn carmichael-numbers])
    (check-true (full-fermat-test cn))))