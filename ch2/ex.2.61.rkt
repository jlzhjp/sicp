#lang racket/base

(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(= x (car set)) #t]
        [(< x (car set)) #f]
        [else (element-of-set? x (cdr set))]))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ([x1 (car set1)]
            [x2 (car set2)])
        (cond [(= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2)))]
              [(< x1 x2) (intersection-set (cdr set1) set2)]
              [(< x2 x1) (intersection-set set1  (cdr set2))]))))

(define (adjoin-set x set)
  (cond [(null? set) (cons x set)]
        [(< x (car set)) (cons x set)]
        [(> x (car set)) (cons (car set) (adjoin-set x (cdr set)))]
        [else set]))

(module+ test
  (require rackunit)

  (check-equal? (intersection-set '(1 2 3 4) '(2 3 4 5)) '(2 3 4))
  (check-equal? (adjoin-set 3 '(1 2 4 5)) '(1 2 3 4 5)))