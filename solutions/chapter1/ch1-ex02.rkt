#lang racket/base

(define expr
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))

(module+ test
  (require rackunit)

  (check-= expr (- (/ 37 150)) 0))