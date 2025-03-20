#lang racket/base

(provide last-pair)

(require compatibility/mlist)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)

(module+ test
  (require sicp-lib/testing)

  (define x (list 'a 'b))
  (define y (list 'c 'd))
  (define z (append x y))
  (check-equal? z '(a b c d))
  (check-equal? (cdr x) '(b))

  (define mx (mlist 'a 'b))
  (define my (mlist 'c 'd))
  (define mw (append! mx my))
  (check-equal? mw (mlist 'a 'b 'c 'd))
  (check-equal? (mcdr mx) (mlist 'b 'c 'd)))
