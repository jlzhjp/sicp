#lang racket/base

(provide list->tree)

(require "ch2-ex63.rkt")

(define (list->tree elements)
  (let-values ([(tree _)
                (partial-tree elements (length elements))])
    tree))

(define (partial-tree elts n)
  (if (= n 0)
      (values '() elts)
      (let ([left-size (quotient (- n 1) 2)])
        (let-values ([(left-tree non-left-elts)
                      (partial-tree elts left-size)])
          (let ([this-entry (car non-left-elts)]
                [right-size (- n (+ left-size 1))])
            (let-values ([(right-tree remaining-elts)
                          (partial-tree (cdr non-left-elts) right-size)])
              (values (make-tree this-entry left-tree right-tree)
                      remaining-elts)))))))

(module+ test
  (require rackunit)

  (check-equal?
   (list->tree
    '(1 3 5 7 9 11))
   '(5 (1 ()
          (3 () ()))
       (9 (7 () ())
          (11 () ())))))