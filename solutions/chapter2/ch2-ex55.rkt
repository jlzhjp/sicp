#lang racket/base

(module+ main
  (displayln (car ''abc)))

#|
"'" 是一种语法糖

(car ''abc)
=> (car (quote (quote abc)))
=> (car '(quote abc))
=> 'quote
|#

(module+ test)