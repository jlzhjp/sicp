#lang racket/base

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


(module+ test
  (require akari-sicp/lib/testing)

  (check-= (count-pairs (cons '() (cons '() (cons '() '()))))
           3
           0)

  (define y (cons '() '()))
  (define z (cons y (cons y '())))

  (check-= (count-pairs z)
           4
           0)

  (define a (cons '() '()))
  (define b (cons a a))
  (define c (cons b b))

  (check-= (count-pairs c)
           7
           0)

  #| 有环的情况下不返回 |#)