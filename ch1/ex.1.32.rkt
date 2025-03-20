#lang racket/base

(require sicp-lib)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate* combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum* term a next b)
  (accumulate* + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (product* term a next b)
  (accumulate* * 1 term a next b))

(module+ test
  (require rackunit)

  (check-= (sum square 1 inc 10) 385 0)
  (check-= (sum* square 1 inc 10) 385 0)
  (check-= (product square 1 inc 10) 13168189440000 0)
  (check-= (product* square 1 inc 10) 13168189440000 0))
