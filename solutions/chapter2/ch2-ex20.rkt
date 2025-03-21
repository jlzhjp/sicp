#lang racket/base

(define (same-parity . xs)
  (define (numbers-of-parity parity xs)
    (cond [(null? xs) '()]
          [(= (remainder (car xs) 2) parity)
           (cons (car xs) (numbers-of-parity parity (cdr xs)))]
          [else (numbers-of-parity parity (cdr xs))]))
  (numbers-of-parity (remainder (car xs) 2) xs))

(module+ test
  (require rackunit)

  (check-equal? (same-parity 1 2 3 4 5 6 7) '(1 3 5 7))
  (check-equal? (same-parity 2 3 4 5 6 7) '(2 4 6)))