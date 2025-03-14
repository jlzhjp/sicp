#lang racket/base

(require (only-in support square))

(define (tree-map proc tree)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (proc tree)]
        [else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree)))]))

(define (square-tree tree) (tree-map square tree))

(module+ test
  (require rackunit)

  (check-equal? (square-tree
                 (list 1
                       (list 2 (list 3 4) 5)
                       (list 6 7)))
                (list 1 (list 4 (list 9 16) 25) (list 36 49))))
