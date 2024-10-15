#lang racket/base

(require "../common.rkt")

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x) (* x x x))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (cond [(or (= k 0) (= k n)) (y k)]
          [(even? k) (* 2.0 (y k))]
          [else (* 4.0 (y k))]))
  (* (/ h 3) (sum term 0 inc n)))

(module+ test
  (require rackunit)

  (check-= (integral cube 0 1 0.01) 0.25 0.001)
  (check-= (integral cube 0 1 0.001) 0.25 0.000001)
  (check-= (simpson-integral cube 0 1 100) 0.25 0.000000000000001)
  (check-= (simpson-integral cube 0 1 1000) 0.25 0.000000000000001))

(module+ main
  (displayln (format "integral, dx=0.01: ~a" (integral cube 0 1 0.01)))
  (displayln (format "integral, dx=0.001: ~a" (integral cube 0 1 0.001)))
  (displayln (format "simpson-integral, n=100: ~a" (simpson-integral cube 0 1 100)))
  (displayln (format "simpson-integral, n=1000: ~a" (simpson-integral cube 0 1 1000))))