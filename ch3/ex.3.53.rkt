#lang racket/base

(provide add-streams)

(require "ex.3.50.rkt")

(define (add-streams s1 s2) (stream-map + s1 s2))

(module+ test
  (require support/stream
           support/testing)

  (define s (cons-stream 1 (add-streams s s)))

  ; s:   1 2 4 ...
  ; s:   1 2 4 ...
  ; s: 1 2 4 8 ...
  (check-stream-prefix-=
   s
   '(1 2 4 8 16)))