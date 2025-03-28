#lang racket/base

(require (only-in akari-sicp/lib/common square))

(define (square-tree tree)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (square tree)]
        [else (cons (square-tree (car tree))
                    (square-tree (cdr tree)))]))

(define (square-tree* tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree* sub-tree)
             (square sub-tree)))
       tree))

(module+ test
  (require rackunit)

  (check-equal? (square-tree
                 (list 1
                       (list 2 (list 3 4) 5)
                       (list 6 7)))
                (list 1 (list 4 (list 9 16) 25) (list 36 49)))

  (check-equal? (square-tree*
                 (list 1
                       (list 2 (list 3 4) 5)
                       (list 6 7)))
                (list 1 (list 4 (list 9 16) 25) (list 36 49))))
