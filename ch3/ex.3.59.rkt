#lang racket/base

(provide sine-series
         cosine-series)

(require support/stream
         (only-in "ex.3.50.rkt" stream-map))

; Exercise 3.59:

; a. Define a procedure integrate-series
; that takes as input a stream a_0, a_1, a_2, ... representing
; a power series and returns the stream a_0, \frac12 a_1,
; \frac13 a_2, ... of coefficients of the non-constant terms of the integral
; of the series. (Since the result has no constant term, it doesn't
; represent a power series; when we use integrate-series, we will cons
; on the appropriate constant.)

(define (integrate-series s)
  (define (series stream n)
    (cons-stream (/ (stream-car stream) n)
                 (series (stream-cdr stream) (+ n 1))))
  (series s 1))

(module+ test
  (require support/testing)

  ;; Create some test streams
  (define ones (cons-stream 1 ones))

  (define integers
    (cons-stream 1
                 (stream-map + integers ones)))

  (define squares
    (stream-map (lambda (x) (* x x)) integers))

  ;; Test 1: Integrating (1, 1, 1, ...) should give (1, 1/2, 1/3, 1/4, ...)
  (check-stream-prefix-= (integrate-series ones)
                         (list 1 (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5)))

  ;; Test 2: Integrating (1, 2, 3, 4, ...) should give (1, 1, 1, 1, ...)
  (check-stream-prefix-= (integrate-series integers)
                         (list 1 1 1 1 1))

  ;; Test 3: Integrating (1, 4, 9, 16, ...) should give (1, 2, 3, 4, ...)
  (check-stream-prefix-= (integrate-series squares)
                         (list 1 2 3 4 5)))

; b. The function x |-> e^x is its own derivative. This implies
; that e^x and the integral of e^x are the same series,
; except for the constant term, which is e^0 = 1.
; Accordingly, we can generate the series for e^x as

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(module+ test
  ;; exp-series should generate the power series for e^x: [1, 1, 1/2, 1/6, 1/24, 1/120, ...]
  (check-stream-prefix-= exp-series
                         (list 1 1 (/ 1 2) (/ 1 6) (/ 1 24) (/ 1 120))))

; Show how to generate the series for sine and cosine,
; starting from the facts that the derivative of sine is
; cosine and the derivative of cosine is the negative of
; sine:

(define (neg x) (- x))

(define cosine-series
  (cons-stream 1 (stream-map neg (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(module+ test
  ;; The power series for sine: [0, 1, 0, -1/6, 0, 1/120, 0, -1/5040, ...]
  ;; sine(x) = 0 + x - 0·x²/2! - x³/3! + 0·x⁴/4! + x⁵/5! - ...
  (check-stream-prefix-= sine-series
                         (list 0 1 0 (/ -1 6) 0 (/ 1 120) 0 (/ -1 5040)))

  ;; The power series for cosine: [1, 0, -1/2, 0, 1/24, 0, -1/720, ...]
  ;; cos(x) = 1 + 0·x - x²/2! + 0·x³/3! + x⁴/4! - 0·x⁵/5! - x⁶/6! + ...
  (check-stream-prefix-= cosine-series
                         (list 1 0 (/ -1 2) 0 (/ 1 24) 0 (/ -1 720))))