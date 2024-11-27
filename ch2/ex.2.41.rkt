#lang racket/base

(require (only-in support
                  dec
                  flatmap
                  enumerate-interval))


(define (unique-triplets n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (dec j))))
                      (enumerate-interval 1 (dec i))))
           (enumerate-interval 1 n)))

(define (triplets-sum-to s n)
  (filter (lambda (t) (= (apply + t) s)) (unique-triplets n)))

(module+ test
  (require rackunit)
  (check-equal? (triplets-sum-to 10 8) '((5 3 2) (5 4 1) (6 3 1) (7 2 1))))