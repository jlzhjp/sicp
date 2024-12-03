#lang racket/base

(require "ex.3.50.rkt"
         "ex.3.53.rkt"
         support/stream)

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (mul-streams s1 s2) (stream-map * s1 s2))

;   integers:   2 3  4   5 ...
; factorials:   1 2  6  24 ...
; factorials: 1 2 6 24 120 ...
(define factorials
  (cons-stream 1
               (mul-streams (stream-cdr integers)
                            factorials)))

(module+ test
  (require support/testing)

  (check-stream-prefix-=
   factorials
   '(1 2 6 24 120 720)))