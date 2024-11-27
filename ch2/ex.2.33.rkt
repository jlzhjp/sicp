#lang racket/base

(require (only-in support
                  accumulate
                  square
                  inc))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (_ len) (inc len)) 0 sequence))

(module+ test
  (require rackunit)

  (define lst '(1 2 3))
  (check-equal? (map square lst) '(1 4 9))
  (check-equal? (append lst '(4 5 6)) '(1 2 3 4 5 6))
  (check-= (length lst) 3 0))