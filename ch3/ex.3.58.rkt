#lang racket/base

(require support/stream)

; Exercise 3.58: Give an interpretation of the stream computed by the following procedure
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(module+ main
  ; (1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8)
  (displayln (collect-stream (expand 1 7 10) 16))
  ; (3 7 5 0 0 0 0 0 0 0 0 0 0 0 0 0)
  (displayln (collect-stream (expand 3 8 10) 16)))

; 1/7 = 0.142857...
; 3/8 = 0.375...