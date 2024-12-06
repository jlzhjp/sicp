#lang racket/base

(require support)

(define (check-cycle x)
  (define (check-cycle-inner x1 x2)
    (cond [(eq? x1 x2) #t]
          [(or (null? x2) (null? (mcdr x2))) #f]
          [else (check-cycle-inner (mcdr x1) (mcddr x2))]))
  (cond [(or (null? x) (null? (mcdr x))) #f]
        [else (check-cycle-inner (mcdr x) (mcddr x))]))

(module+ test
  (require support/testing)

  (check-false (check-cycle (mcons '() (mcons '() (mcons '() '())))))

  (define y (mcons '() '()))
  (define z (mcons y (mcons y '())))

  (check-false (check-cycle z))

  (define a (mcons '() '()))
  (define b (mcons a a))
  (define c (mcons b b))

  (check-false (check-cycle c))

  (define cycle-list (mcons '() (mcons '() (mcons '() '()))))
  (set-mcdr! (mcddr cycle-list) cycle-list)
  (check-true (check-cycle cycle-list)))
