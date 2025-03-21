#lang racket/base

(provide integers)

(require akari-sicp/lib/stream
         "ch3-ex50.rkt"
         "ch3-ex53.rkt")

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

; Exercise 3.54
; Define the procedure mul-streams, analogous to add-streams, that
; proceduces the elementwise product of its two input streams.
(define (mul-streams s1 s2) (stream-map * s1 s2))

; Use mul-streams together with the stream of integers to complete the
; definition of the stream of factorials.

;   integers:   2 3  4   5 ...
; factorials:   1 2  6  24 ...
; factorials: 1 2 6 24 120 ...
(define factorials
  (cons-stream 1
               (mul-streams (stream-cdr integers)
                            factorials)))

(module+ test
  (require akari-sicp/lib/testing)

  (check-stream-prefix-=
   factorials
   '(1 2 6 24 120 720)))