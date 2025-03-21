#lang racket/base

(require "ch2-ex02.rkt")

(define (make-rectangle1 top-left-point width height)
  (list 'rectangle-point-side-length top-left-point width height))
(define width-rectangle1 caddr)
(define height-rectangle1 cadddr)

(define (make-rectangle2 top-left-point bottom-right-point)
  (list 'rectangle-point-point top-left-point bottom-right-point))
(define top-left-point-rectangle2 cadr)
(define bottom-right-point-rectangle2 caddr)

(define (width-rectangle2 rectangle)
  (- (x-point (bottom-right-point-rectangle2 rectangle))
     (x-point (top-left-point-rectangle2 rectangle))))

(define (height-rectangle2 rectangle)
  (- (y-point (bottom-right-point-rectangle2 rectangle))
     (y-point (top-left-point-rectangle2 rectangle))))

(define (width-rectangle rectangle)
  (let ([type-tag (car rectangle)])
    ((cond [(eqv? type-tag 'rectangle-point-side-length) width-rectangle1]
           [(eqv? type-tag 'rectangle-point-point) width-rectangle2]
           [else (error "unexpected type tag " type-tag)])
     rectangle)))

(define (height-rectangle rectangle)
  (let ([type-tag (car rectangle)])
    ((cond [(eqv? type-tag 'rectangle-point-side-length) height-rectangle1]
           [(eqv? type-tag 'rectangle-point-point) height-rectangle2]
           [else (error "unexpected type tag " type-tag)])
     rectangle)))

(define (area-rectangle rectangle)
  (* (width-rectangle rectangle)
     (height-rectangle rectangle)))

(define (perimeter-rectangle rectangle)
  (* 2
     (+ (width-rectangle rectangle)
        (height-rectangle rectangle))))

(module+ test
  (require rackunit)

  (define rect1 (make-rectangle1 (make-point 0 0) 20 10))
  (define rect2 (make-rectangle2 (make-point 1 2) (make-point 5 4)))

  (check-= (area-rectangle rect1) 200 0)
  (check-= (area-rectangle rect2) 8 0)

  (check-= (perimeter-rectangle rect1) 60 0)
  (check-= (perimeter-rectangle rect2) 12 0))