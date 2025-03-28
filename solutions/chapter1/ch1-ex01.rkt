#lang racket/base

(module+ test
  (require akari-sicp/lib/testing)

  (define a 3)
  (define b (+ a 1))

  (run-tests
   (describe "Exercise 1.1"
     (it.eqv? 10 10 "10")
     (it.eqv? (+ 5 3 4) 12 "(+ 5 3 4)")
     (it.eqv? (- 9 1) 8 "(- 9 1)")
     (it.eqv? (/ 6 2) 3 "(/ 6 2)")
     (it.eqv? (/ 6 2) 3 "(/ 6 2)")
     (it.eqv? (+ (* 2 4) (- 4 6)) 6 "(+ (* 2 4) (- 4 6))")
     (it.eqv? (+ a b (* a b)) 19 "(+ a b (* a b))")
     (it.false (= a b) "(= a b)")
     (it.eqv? (if (and (> b a) (< b (* a b)))
                  b
                  a)
              b
              "(if (and (> b a) (< b (* a b))) b a)")
     (it.eqv? (cond [(= a 4) 6]
                    [(= b 4) (+ 6 7 a)]
                    [else 25])
              16
              "(cond [(= a 4) 6] [(= b 4) (+ 6 7 a)] [else 25])")
     (it.eqv? (+ 2 (if (> b a) b a)) 6 "(+ 2 (if (> b a) b a))")
     (it.eqv? (* (cond [(> a b) a]
                       [(< a b) b]
                       [else -1])
                 (+ a 1))
              16
              "(* (cond [(> a b) a] [(< a b) b] [else -1]) (+ a 1))"))))
