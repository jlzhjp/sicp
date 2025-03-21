#lang racket/base

(require compatibility/mlist)

(define (set-to-wow! x)
  (set-mcar! (mcar x) 'wow)
  x)

(define x (mlist 'a 'b))
(define z1 (mcons x x))

(define z2 (mcons (mlist 'a 'b) (mlist 'a 'b)))

(module+ test
  (require akari-sicp/lib/testing)

  (check-equal? (set-to-wow! z1)
                (mlist (mlist 'wow 'b) 'wow 'b))
  (check-equal? (set-to-wow! z2)
                (mlist (mlist 'wow 'b) 'a 'b)))