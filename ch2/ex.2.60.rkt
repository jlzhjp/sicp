#lang racket/base

(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(equal? x (car set)) #t]
        [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond [(or (null? set1) (null? set2)) '()]
        [(element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2))]
        [else (intersection-set (cdr set1) set2)]))

(module+ test
  (require rackunit)

  (define set1 '(1 2 3 4 5 6 7))
  (define set2 '(4 5 6 7 8 9 10))

  (check-false (element-of-set? 8 set1))
  (check-true (element-of-set? 7 set1))
  (check-equal? (adjoin-set 8 set1) '(8 1 2 3 4 5 6 7))
  (check-equal? (adjoin-set 7 set1) '(7 1 2 3 4 5 6 7))
  (check-equal? (intersection-set set1 set2) '(4 5 6 7))
  (check-equal? (union-set set1 set2) '(1 2 3 4 5 6 7 4 5 6 7 8 9 10)))