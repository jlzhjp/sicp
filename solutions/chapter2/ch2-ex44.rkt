#lang racket/base

(provide square-limit)

(require (only-in "ch2-ex50.rkt" flip-horiz flip-vert)
         (only-in "ch2-ex51.rkt" beside below))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (right-split painter (- n 1))])
        (beside painter (below smaller smaller)))))


(define (up-split painter n)
  (if (= n 0)
      painter
      (let ([smaller (up-split painter (- n 1))])
        (below painter (beside smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let* ([up (up-split painter (- n 1))]
             [right (right-split painter (- n 1))]
             [top-left (beside up up)]
             [bottom-right (below right right)]
             [corner (corner-split painter (- n 1))])
        (beside (below painter top-left)
                (below bottom-right corner)))))


(define (square-limit painter n)
  (let* ([quarter (corner-split painter n)]
         [half (beside (flip-horiz quarter) quarter)])
    (below (flip-vert half) half)))
