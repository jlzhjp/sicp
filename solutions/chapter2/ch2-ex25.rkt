#lang racket/base

(define lst1 (list 1 3 (list 5 7) 9))
(define lst2 (list (list 7)))
(define lst3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

; (displayln lst1)
; (displayln lst2)
; (displayln lst3)

(module+ test
  (require rackunit)

  (check-= (car (cdr (car (cdr (cdr lst1))))) 7 0)
  (check-= (car (car lst2)) 7 0)
  (check-= (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr lst3)))))))))))) 7 0))