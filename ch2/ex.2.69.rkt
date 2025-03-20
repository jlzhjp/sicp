#lang racket/base

(provide generate-huffman-tree)

(require "ex.2.67.rkt")

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(< (weight x) (weight (car set))) (cons x set)]
        [else (cons (car set)
                    (adjoin-set x (cdr set)))]))

; pairs: ((A 4) (B 2) (C 1) (D 1))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ([pair (car pairs)])
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (define (successive-merge leaf-set)
    (if (null? (cdr leaf-set))
        (car leaf-set)
        (successive-merge (adjoin-set (make-code-tree (car leaf-set)
                                                      (cadr leaf-set))
                                      (cddr leaf-set)))))
  (successive-merge (make-leaf-set pairs)))

(module+ test
  (require sicp-lib/testing)

  (define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                     (make-leaf 'B 2)
                     (make-code-tree (make-leaf 'D 1)
                                     (make-leaf 'C 1)))))
  (check-equal? (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
                sample-tree))