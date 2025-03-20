#lang racket/base

(provide make-point
         x-point
         y-point
         print-point
         make-segment
         start-segment
         end-segment)

(require (only-in sicp-lib average))

(define (make-point x y) (list 'point x y))
(define x-point cadr)
(define y-point caddr)

(define (print-point point)
  (display (format "(~a, ~a)" (x-point point) (y-point point))))

(define (make-segment start end) (list 'segment start end))
(define start-segment cadr)
(define end-segment caddr)

(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment))
                       (x-point (end-segment segment)))
              (average (y-point (start-segment segment))
                       (y-point (end-segment segment)))))

(module+ test
  (require rackunit)
  (define test-segment (make-segment (make-point 1 2) (make-point 7 4)))

  (check-equal? (midpoint-segment test-segment) (make-point 4 3)))