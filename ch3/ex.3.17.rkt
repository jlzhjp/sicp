#lang racket/base

(require compatibility/mlist)

(define (count-pairs x)
  (define visited '())
  (define (inner node)
    (if (or (mmemq node visited)
            (not (mpair? node)))
        0
        (begin
          (set! visited (mcons node visited))
          (+ (inner (mcar node))
             (inner (mcdr node))
             1))))
  (inner x))

(module+ test
  (require support
           support/testing)

  (check-= (count-pairs (mcons '() (mcons '() (mcons '() '()))))
           3
           0)

  (define y (mcons '() '()))
  (define z (mcons y (mcons y '())))

  (check-= (count-pairs z)
           3
           0)

  (define a (mcons '() '()))
  (define b (mcons a a))
  (define c (mcons b b))

  (check-= (count-pairs c)
           3
           0)

  (define cycle-list (mcons '() (mcons '() (mcons '() '()))))
  (set-mcdr! (mcddr cycle-list) cycle-list)
  (check-= (count-pairs cycle-list)
           3
           0))