#lang racket/base

(define (equal? x y)
  (cond [(or (not (pair? x)) (not (pair? y))) (eqv? x y)]
        [(equal? (car x) (car y)) (equal? (cdr x) (cdr y))]
        [else #f]))

(module+ test
  (require rackunit)

  (check-true (equal? '() '()))
  (check-false (equal? '() 1))
  (check-false (equal? '(1 2) '(1 2 3)))
  (check-true (equal? '(1 2 3) '(1 2 3)))
  (check-true (equal? '(this (is a) list) '(this (is a) list)))
  (check-false (equal? '(this is a list) '(this (is a) list))))