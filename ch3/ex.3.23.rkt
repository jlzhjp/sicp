#lang racket/base

(require compatibility/mlist
         support)

(define (make-deque) (mcons '() '()))

(define front-node-ptr mcar)
(define rear-node-ptr mcdr)

(define (set-front-node-ptr! deque new-node)
  (set-mcar! deque new-node))
(define (set-rear-node-ptr! deque new-node)
  (set-mcdr! deque new-node))

(define (make-deque-node val prev next) (mlist val prev next))

(define (val-deque-node node) (mcar node))
(define (prev-deque-node node) (mcadr node))
(define (next-deque-node node) (mcaddr node))

(define (set-deque-node-val! node new-val)
  (set-mcar! node new-val))
(define (set-deque-node-prev! node new-prev)
  (set-mcar! (mcdr node) new-prev))
(define (set-deque-node-next! node new-next)
  (set-mcar! (mcddr node) new-next))

(define (empty-deque? deque) (null? (front-node-ptr deque)))
(define (front-deque deque)
  (if (null? deque)
      (error "FRONT call with an empty deque" deque)
      (val-deque-node (front-node-ptr deque))))

(define (rear-deque deque)
  (if (null? deque)
      (error "REAR call with a empty deque" deque)
      (val-deque-node (rear-node-ptr deque))))

(define (front-insert-deque! deque item)
  (let ([new-node (make-deque-node item '() '())])
    (cond [(empty-deque? deque)
           (set-front-node-ptr! deque new-node)
           (set-rear-node-ptr! deque new-node)]
          [else
           (set-deque-node-prev! (front-node-ptr deque) new-node)
           (set-deque-node-next! new-node (front-node-ptr deque))
           (set-front-node-ptr! deque new-node)])))

(define (rear-insert-deque! deque item)
  (let ([new-node (make-deque-node item '() '())])
    (cond [(empty-deque? deque)
           (front-insert-deque! deque item)]
          [else
           (set-deque-node-next! (rear-node-ptr deque) new-node)
           (set-deque-node-prev! new-node (rear-node-ptr deque))
           (set-rear-node-ptr! deque new-node)])))

(define (front-delete-deque! deque)
  (cond [(empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque")]
        [else
         (set-front-node-ptr!
          deque
          (next-deque-node (front-node-ptr deque)))]))

(define (rear-delete-deque! deque)
  (cond [(empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque")]
        [else
         (set-rear-node-ptr!
          deque
          (prev-deque-node (rear-node-ptr deque)))]))

(define (print-deque deque)
  (define (iter node)
    (cond [(null? node) '()]
          [(eq? (rear-node-ptr deque) node)
           (display " ")
           (display (val-deque-node node))]
          [else
           (display " ")
           (display (val-deque-node node))
           (iter (next-deque-node node))]))
  (display "(deque")
  (iter (front-node-ptr deque))
  (display ")"))

(module+ test
  (require support/testing)

  (define d (make-deque))

  (check-normalized-output
   (lambda ()
     (print-deque d) (newline)
     (front-insert-deque! d 'b)
     (print-deque d) (newline)
     (front-insert-deque! d 'a)
     (print-deque d) (newline)
     (front-insert-deque! d 0)
     (print-deque d) (newline)
     (rear-insert-deque! d 'c)
     (print-deque d) (newline)
     (rear-insert-deque! d 'd)
     (print-deque d) (newline)
     (rear-insert-deque! d 0)
     (print-deque d) (newline)
     (rear-delete-deque! d)
     (print-deque d) (newline)
     (front-delete-deque! d)
     (print-deque d) (newline))

   '("(deque)"
     "(deque b)"
     "(deque a b)"
     "(deque 0 a b)"
     "(deque 0 a b c)"
     "(deque 0 a b c d)"
     "(deque 0 a b c d 0)"
     "(deque 0 a b c d)"
     "(deque a b c d)")))
