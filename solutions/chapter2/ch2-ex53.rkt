#lang racket/base

(define (memq item x)
  (cond [(null? x) #f]
        [(eqv? item (car x)) x]
        [else (memq item (cdr x))]))

(module+ test
  (require rackunit)

  (check-equal? (list 'a 'b 'c) '(a b c))
  (check-equal? (list (list 'george)) '((george)))
  (check-equal? (cdr '((x1 x2) (y1 y2))) '((y1 y2)))
  (check-equal? (cadr '((x1 x2) (y1 y2))) '(y1 y2))
  (check-false (pair? (car '(a short list))))
  (check-false (memq 'red '((red shoes) (blue socks))))
  (check-equal? (memq 'red '(red shoes blue socks)) '(red shoes blue socks)))
