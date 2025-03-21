#lang racket/base

(provide make-vect
         xcor-vect
         ycor-vect
         add-vect
         sub-vect
         scale-vect)

(define (make-vect x y) (list x y))
(define xcor-vect car)
(define ycor-vect cadr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(module+ test
  (require rackunit)

  (define v1 (make-vect 1 2))
  (define v2 (make-vect 3 4))

  (check-equal? (add-vect v1 v2) (make-vect 4 6))
  (check-equal? (sub-vect v2 v1) (make-vect 2 2))
  (check-equal? (scale-vect 2 v2) (make-vect 6 8)))
