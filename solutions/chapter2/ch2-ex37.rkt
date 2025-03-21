#lang racket/base

(require (only-in akari-sicp/lib/common accumulate)
         (only-in "ch2-ex36.rkt" accumulate-n))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(module+ test
  (require rackunit)

  (define matrix '((1 2 3 4)
                   (4 5 6 6)
                   (6 7 8 9)))
  (define matrix2 '((1 2 3)
                    (4 5 6)
                    (7 8 9)
                    (10 11 12)))
  (define vector '(1 2 3 4))

  (check-equal? (matrix-*-vector matrix vector)
                '(30 56 80))
  (check-equal?(transpose matrix)
               '((1 4 6) (2 5 7) (3 6 8) (4 6 9)))
  (check-equal?(matrix-*-matrix matrix matrix2)
               '((70 80 90) (126 147 168) (180 210 240))))