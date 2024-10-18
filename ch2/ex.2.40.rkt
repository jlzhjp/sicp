#lang racket/base

(require (only-in "../ch1/ex.1.22.rkt"
                  prime?)
         (only-in "../common.rkt"
                  flatmap
                  enumerate-interval
                  dec))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (dec i))))
           (enumerate-interval 1 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(module+ test
  (require rackunit)

  (check-equal? (prime-sum-pairs 6)
                '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))))