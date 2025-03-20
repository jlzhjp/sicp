#lang racket/base

(require sicp-lib)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items '()))

(define (square-list* items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

(module+ test)

(module+ main
  (displayln (square-list '(1 2 3 4)))
  (displayln (square-list* '(1 2 3 4))))

#|
(16 9 4 1)
((((() . 1) . 4) . 9) . 16)
|#