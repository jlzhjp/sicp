#lang racket/base

(require (only-in sicp-lib inc)
         (only-in "../ch1/ex.1.43.rkt" repeated)
         (only-in "../ch1/ex.1.42.rkt" compose))

(define zero (lambda (_) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) ((repeated f 2) x))))

(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add m n) (lambda (f) (compose (m f) (n f))))

(define (church->natural church)
  ((church inc) 0))

(module+ test
  (require rackunit)

  (check-= (church->natural zero) 0 0)
  (check-= (church->natural (add-1 zero)) 1 0)
  (check-= (church->natural one) 1 0)
  (check-= (church->natural two) 2 0)
  (check-= (church->natural (add one two)) 3 0))