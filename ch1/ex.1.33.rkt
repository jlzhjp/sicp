#lang racket/base

(require sicp-lib)
(require (only-in "ex.1.22.rkt" prime?))

(define (filtered-accumulate combiner pred? null-value term a next b)
  (if (> a b)
      null-value
      (let ([term-a (term a)])
        (if (pred? term-a)
            (combiner term-a
                      (filtered-accumulate combiner pred? null-value term (next a) next b))
            (filtered-accumulate combiner pred? null-value term (next a) next b)))))

(define (filtered-accumulate* combiner pred? null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (let ([term-a (term a)])
          (if (pred? term-a)
              (iter (next a) (combiner result term-a))
              (iter (next a) result)))))
  (iter a null-value))

(define (sum-of-primes a b)
  (filtered-accumulate + prime? 0 identity a inc b))

(define (sum-of-primes* a b)
  (filtered-accumulate* + prime? 0 identity a inc b))

(module+ test
  (require rackunit)

  (check-= (sum-of-primes 10 15) (+ 11 13) 0)
  (check-= (sum-of-primes* 10 15) (+ 11 13) 0))


(define (product-of-relative-primes n)
  (define (relative-prime? i) (= (gcd n i) 1))
  (filtered-accumulate * relative-prime? 1 identity 2 inc (dec n)))

(define (product-of-relative-primes* n)
  (define (relative-prime? i) (= (gcd n i) 1))
  (filtered-accumulate* * relative-prime? 1 identity 2 inc (dec n)))

(module+ test
  (check-= (product-of-relative-primes 10) (* 3 7 9) 0)
  (check-= (product-of-relative-primes* 10) (* 3 7 9) 0))