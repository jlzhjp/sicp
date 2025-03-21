#lang racket/base

(define (subsets s)
  (if (null? s)
      (list '())
      (let ([rest (subsets (cdr s))])
        (append rest
                (map (lambda (a) (cons (car s) a))
                     rest)))))

(module+ test
  (require rackunit)

  (check-equal? (subsets '(1 2 3))
                '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))))