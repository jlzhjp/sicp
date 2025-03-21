#lang racket/base

(require compatibility/mlist)

(define (check-cycle x)
  (define visited '())
  (define (check-cycle-inner node)
    (cond [(mmemq node visited) #t]
          [(not (mpair? node)) #f]
          [else (begin (set! visited (mcons node visited))
                       (check-cycle-inner (mcdr node)))]))
  (check-cycle-inner x))

(module+ test
  (require akari-sicp/lib/common
           akari-sicp/lib/testing)

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

