#lang racket/base

(provide entry
         left-branch
         right-branch
         make-tree
         tree->list-1
         tree->list-2)

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond [(null? set) #f]
        [(= x (entry set)) #t]
        [(< x (entry set))
         (element-of-set? x (left-branch set))]
        [(> x (entry set))
         (element-of-set? x (right-branch set))]))

(define (adjoin-set x set)
  (cond [(null? set) (make-tree x '() '())]
        [(= x (entry set)) set]
        [(< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set))]
        [(> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set)))]))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define tree-1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))

(define tree-2
  (make-tree 3
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5
                                   '()
                                   '())
                        (make-tree 9
                                   '()
                                   (make-tree 11
                                              '()
                                              '())))))

(define tree-3
  (make-tree 5
             (make-tree 3
                        (make-tree 1
                                   '()
                                   '())
                        '())
             (make-tree 9
                        (make-tree 7
                                   '()
                                   '())
                        (make-tree 11
                                   '()
                                   '()))))

(module+ test
  (require rackunit)

  (check-equal? (tree->list-1 tree-1) '(1 3 5 7 9 11))
  (check-equal? (tree->list-1 tree-2) '(1 3 5 7 9 11))
  (check-equal? (tree->list-1 tree-3) '(1 3 5 7 9 11))
  (check-equal? (tree->list-2 tree-1) '(1 3 5 7 9 11))
  (check-equal? (tree->list-2 tree-2) '(1 3 5 7 9 11))
  (check-equal? (tree->list-2 tree-3) '(1 3 5 7 9 11)))