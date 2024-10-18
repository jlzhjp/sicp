#lang racket/base

(define (make-mobile left right)
  (list left right))
(define left-branch car)
(define right-branch cadr)

(define (make-branch length structure)
  (list length structure))
(define branch-length car)
(define branch-structure cadr)

; 如果改变活动体的表示，只需修改选择函数即可
; (define (make-mobile left right)
;   (cons left right))
; (define left-branch car)
; (define right-branch cdr)

; (define (make-branch length structure)
;   (cons length structure))
; (define branch-length car)
; (define branch-structure car)


(define mobile-x
  (make-mobile (make-branch 1 (make-mobile (make-branch 1 1)
                                           (make-branch 1 2)))
               (make-branch 1 (make-mobile (make-branch 1 3)
                                           (make-branch 1 4)))))

(define mobile-balanced
  (make-mobile (make-branch 2 (make-mobile (make-branch 1 2)
                                           (make-branch 1 2)))
               (make-branch 1 (make-mobile (make-branch 1 4)
                                           (make-branch 1 4)))))

(define (total-weight x)
  (if (pair? x)
      (+ (total-weight (branch-structure (left-branch x)))
         (total-weight (branch-structure (right-branch x))))
      x))

(define (balanced? x)
  (define (helper x)
    (if (pair? x)
        (let-values ([(left-balanced left-weight)
                      (helper (branch-structure (left-branch x)))]
                     [(right-balanced right-weight)
                      (helper (branch-structure (right-branch x)))])
          (values (and left-balanced
                       right-balanced
                       (= (* left-weight (branch-length (left-branch x)))
                          (* right-weight (branch-length (right-branch x)))))
                  (+ left-weight right-weight)))
        (values #t x)))
  (let-values ([(balanced _) (helper x)]) balanced))

(module+ test
  (require rackunit)

  (check-= (total-weight mobile-x) 10 0)
  (check-= (total-weight mobile-balanced) 12 0)
  (check-false (balanced? mobile-x))
  (check-true (balanced? mobile-balanced)))