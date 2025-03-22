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
  (require akari-sicp/lib/testing)

  (define v1 (make-vect 1 2))
  (define v2 (make-vect 3 4))

  (run-tests
   (describe "vector operations"
     (it.equal? "should add vectors correctly" (add-vect v1 v2) (make-vect 4 6))
     (it.equal? "should subtract vectors correctly" (sub-vect v2 v1) (make-vect 2 2))
     (it.equal? "should scale vectors correctly" (scale-vect 2 v2) (make-vect 6 8)))))
