#lang racket/base

(require (prefix-in a: "ex.2.58.a.rkt")
         (prefix-in b: "ex.2.58.b.rkt"))


(module+ test
  (require rackunit)

  (check-equal? (a:deriv '(x + (3 * (x + (y + 2)))) 'x) 4)
  (check-equal? (b:deriv '(x + 3 * (x + y + 2)) 'x) 4)
  (check-equal? (b:deriv '(3 * (x + y * 2) + x + 1) 'x) 4))