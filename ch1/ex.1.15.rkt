#lang racket/base

(require racket/match)
(require racket/math)

; if x is sufficiently small, sin x = x
; sin x = 3 sin (x / 3) - 4 sin^3 (x / 3)

(define (make-counted function)
  (define count 0)
  (define (increase-counter) (set! count (+ count 1)))
  (define (get-counter-value) count)
  (define (counted-function . args)
    (increase-counter)
    (apply function args))
  (values counted-function get-counter-value))

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine p angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine p (/ angle 3.0)))))

(module+ test
  (require rackunit)

  (check-= (sine p pi) 0 0.01)
  (check-= (sine p (/ pi 2)) 1 0.01)

  (define-values (counted-p get-counter-value) (make-counted p))
  (void (sine counted-p 12.15))
  (check-= (get-counter-value) 5 0))