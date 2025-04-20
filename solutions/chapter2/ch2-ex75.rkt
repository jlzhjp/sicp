#lang racket/base

(provide make-from-real-imag
         make-from-mag-ang)

(require akari-sicp/lib/common)

(define (make-from-real-imag x y)
  (define (dispatch op)
    (case op
      [('real-part) x]
      [('imag-part) y]
      [('magnitude) (sqrt (+ (square x) (square y)))]
      [('angle) (atan y x)]
      [else (error 'make-from-real-imag "unknown op ~a" op)]))
  dispatch)

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (case op
      [('real-part) (* mag (cos ang))]
      [('imag-part) (* mag (sin ang))]
      [('magnitude) mag]
      [('angle) ang]
      [else (error 'make-from-mag-ang "unknown op ~a" op)]))
  dispatch)