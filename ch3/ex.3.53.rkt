#lang racket/base

(provide add-streams)

(require "ex.3.50.rkt")

(define (add-streams . argstreams)
  (apply stream-map (cons + argstreams)))

(module+ test
  (require sicp-lib/stream
           sicp-lib/testing)

  ; Exercise 3.53
  ; Without running the program, describe the elements of the stream s.
  (define s (cons-stream 1 (add-streams s s)))

  ; s:   1 2 4 ...
  ; s:   1 2 4 ...
  ; s: 1 2 4 8 ...
  (check-stream-prefix-=
   s
   '(1 2 4 8 16)))