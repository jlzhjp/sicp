#lang racket/base

(provide make-segment
         start-segment
         end-segment)

(define (make-segment start end)
  (list start end))

(define start-segment car)
(define end-segment cadr)


(module+ test
  (require rackunit)
  (require "ex.2.46.rkt")

  (define seg (make-segment (make-vect 1 2)
                            (make-vect 3 4)))
  (check-equal? (start-segment seg) (make-vect 1 2))
  (check-equal? (end-segment seg) (make-vect 3 4)))
