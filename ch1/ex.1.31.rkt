#lang racket/base

(require racket/math)
(require support)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product* term a next b)
  (define (product-iter a result)
    (if (> a b)
        result
        (product-iter (next a) (* (term a) result))))
  (product-iter a 1))

(define (factorial x) (product identity 1 inc x))

(define (factorial* x) (product* identity 1 inc x))

(module+ test
  (require rackunit)

  (check-= (factorial 10) 3628800 0)
  (check-= (factorial* 10) 3628800 0))

(define (pi-num-term i)
  (* 2.0 (floor (/ (+ i 2.0) 2.0))))

(define (pi-den-term i)
  (+ (* 2.0 (floor (/ (+ i 1.0) 2.0))) 1.0))

(define (pi-term i) (/ (pi-num-term i) (pi-den-term i)))

(define (approximate-pi n)
  (* 4.0 (product pi-term 1 inc n)))

(define (approximate-pi* n)
  (* 4.0 (product* pi-term 1 inc n)))

(module+ test
  (check-= (approximate-pi 10000) pi 1e-3)
  (check-= (approximate-pi* 10000) pi 1e-3))
