#lang racket/base

(module+ test)

(module+ main
  (displayln (list 1 (list 2 (list 3 4)))))
; (1 (2 (3 4)))