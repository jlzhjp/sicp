#lang racket/base

(define x (list 1 2 3))
(define y (list 1 2 3))

(module+ test)

(module+ main
  (displayln (append x y))
  #|(1 2 3 1 2 3)|#
  (displayln (cons x y))
  #|((1 2 3) 1 2 3)|#
  (displayln (list x y))
  #|((1 2 3) (1 2 3))|#)