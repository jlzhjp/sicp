#lang racket/base

(provide make-frame
         origin-frame
         edge1-frame
         edge2-frame
         frame-coord-map
         frame-whole-canvas)

(require "ex.2.46.rkt")

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define frame-whole-canvas
  (make-frame (make-vect 0 0)
              (make-vect 1 0)
              (make-vect 0 1)))

(module+ test)