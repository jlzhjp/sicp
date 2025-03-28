#lang racket/base

(define x
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))

(module+ test
  (require akari-sicp/lib/testing)

  (expect [x => (- (/ 37 150))]))