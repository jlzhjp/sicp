#lang racket/base

(define (for-each proc lst)
  (if (null? lst)
      (void)
      (begin (proc (car lst))
             (for-each proc (cdr lst)))))

(module+ test)

(module+ main
  (for-each (lambda (x) (displayln x))
            (list 57 321 88)))