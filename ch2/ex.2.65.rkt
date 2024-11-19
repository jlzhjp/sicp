#lang racket/base

(require (rename-in "ex.2.61.rkt"
                    (intersection-set intersection-set-ordered))
         (rename-in "ex.2.62.rkt"
                    (union-set union-set-ordered))
         "ex.2.63.rkt"
         "ex.2.64.rkt")

(define (make-set elements)
  (list->tree elements))

(define (union-set set1 set2)
  (list->tree (union-set-ordered (tree->list-2 set1)
                                        (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-set-ordered (tree->list-2 set1)
                                        (tree->list-2 set2))))

(module+ test
  (require rackunit)

  (define set1 (make-set '(1 2 3 4 5)))
  (define set2 (make-set '(4 5 6 7 8)))
  (check-equal? (tree->list-2 (union-set set1 set2)) '(1 2 3 4 5 6 7 8))
  (check-equal? (tree->list-2 (intersection-set set1 set2)) '(4 5)))