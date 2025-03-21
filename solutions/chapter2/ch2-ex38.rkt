#lang racket/base

(provide fold-left
         fold-right)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(module+ test
  (require rackunit)

  (check-equal? (fold-right / 1 (list 1 2 3)) (/ 1 (/ 2 (/ 3 1))))
  (check-equal? (fold-left / 1 (list 1 2 3)) (/ (/ (/ 1 1) 2) 3))
  (check-equal? (fold-right list '() (list 1 2 3)) '(1 (2 (3 ()))))
  (check-equal? (fold-left list '() (list 1 2 3)) '(((() 1) 2) 3)))

#|
fold-left:
(((init OP a) OP b) OP c)

fold-right
(a OP (b OP (c OP init)))

1. op 必须满足结合律
2. 对于 initial 和 其他值，op 必须满足交换率（x OP init = init OP x）
|#