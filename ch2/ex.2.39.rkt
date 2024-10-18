#lang racket/base

(require "ex.2.38.rkt")

(define (reverse sequence)
  (fold-right (lambda (x ys) (append ys (list x))) '() sequence))

(define (reverse* sequence)
  (fold-left (lambda (xs y) (cons y xs)) '() sequence))

(module+ test
  (require rackunit)

  (define lst '(1 2 3 4))
  (define rev-lst '(4 3 2 1))
  (check-equal? (reverse lst) rev-lst)
  (check-equal? (reverse* lst) rev-lst))