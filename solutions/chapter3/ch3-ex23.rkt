#lang racket/base

(require akari-sicp/lib/mcons)

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

(define (set-deque-node-prev! node new-prev)
  (set-mcar! (mcdr node) new-prev))
(define (set-deque-node-next! node new-next)
  (set-mcar! (mcddr node) new-next))

(define (empty-deque? deque) (null? (front-node-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT call with an empty deque" deque)
      (val-deque-node (front-node-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
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
        [(eq? (front-node-ptr deque) (rear-node-ptr deque))
         ; If this is the last element, clear both pointers
         (set-front-node-ptr! deque '())
         (set-rear-node-ptr! deque '())]
        [else
         (let ([new-front (next-deque-node (front-node-ptr deque))])
           (set-deque-node-prev! new-front '())
           (set-front-node-ptr! deque new-front))]))

(define (rear-delete-deque! deque)
  (cond [(empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque")]
        [(eq? (front-node-ptr deque) (rear-node-ptr deque))
         ; If this is the last element, clear both pointers
         (set-front-node-ptr! deque '())
         (set-rear-node-ptr! deque '())]
        [else
         (let ([new-rear (prev-deque-node (rear-node-ptr deque))])
           (set-deque-node-next! new-rear '())
           (set-rear-node-ptr! deque new-rear))]))

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
  (require akari-sicp/lib/testing
           rackunit/text-ui)

  (define deque-tests
    (test-suite
     "Tests for deque implementation"

     (test-case "empty-deque? with new deque"
       (let ([d (make-deque)])
         (check-true (empty-deque? d))))

     (test-case "empty-deque? after insertion and deletion"
       (let ([d (make-deque)])
         (front-insert-deque! d 'x)
         (check-false (empty-deque? d))
         (front-delete-deque! d)
         (check-true (empty-deque? d))))

     (test-case "front-insert and front-deque"
       (let ([d (make-deque)])
         (front-insert-deque! d 'a)
         (check-equal? (front-deque d) 'a)
         (front-insert-deque! d 'b)
         (check-equal? (front-deque d) 'b)
         (check-equal? (rear-deque d) 'a)))

     (test-case "rear-insert and rear-deque"
       (let ([d (make-deque)])
         (rear-insert-deque! d 'a)
         (check-equal? (rear-deque d) 'a)
         (rear-insert-deque! d 'b)
         (check-equal? (rear-deque d) 'b)
         (check-equal? (front-deque d) 'a)))

     (test-case "front-delete operation"
       (let ([d (make-deque)])
         (front-insert-deque! d 'a)
         (front-insert-deque! d 'b)
         (front-insert-deque! d 'c)
         (front-delete-deque! d)
         (check-equal? (front-deque d) 'b)))

     (test-case "rear-delete operation"
       (let ([d (make-deque)])
         (rear-insert-deque! d 'a)
         (rear-insert-deque! d 'b)
         (rear-insert-deque! d 'c)
         (rear-delete-deque! d)
         (check-equal? (rear-deque d) 'b)))

     (test-case "single element deque operations"
       (let ([d (make-deque)])
         (front-insert-deque! d 'x)
         (check-equal? (front-deque d) 'x)
         (check-equal? (rear-deque d) 'x)
         (front-delete-deque! d)
         (check-true (empty-deque? d))

         (rear-insert-deque! d 'y)
         (check-equal? (front-deque d) 'y)
         (check-equal? (rear-deque d) 'y)
         (rear-delete-deque! d)
         (check-true (empty-deque? d))))

     (test-case "error conditions"
       (let ([d (make-deque)])
         (check-exn exn:fail? (lambda () (front-deque d)))
         (check-exn exn:fail? (lambda () (rear-deque d)))
         (check-exn exn:fail? (lambda () (front-delete-deque! d)))
         (check-exn exn:fail? (lambda () (rear-delete-deque! d)))))

     (test-case "comprehensive sequence test"
       (check-normalized-output
        (lambda ()
          (let ([d (make-deque)])
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
            (rear-insert-deque! d 'e)
            (print-deque d) (newline)
            (front-delete-deque! d)
            (print-deque d) (newline)
            (rear-delete-deque! d)
            (print-deque d) (newline)
            (front-delete-deque! d)
            (front-delete-deque! d)
            (print-deque d) (newline)))
        '("(deque)"
          "(deque b)"
          "(deque a b)"
          "(deque 0 a b)"
          "(deque 0 a b c)"
          "(deque 0 a b c d)"
          "(deque 0 a b c d e)"
          "(deque a b c d e)"
          "(deque a b c d)"
          "(deque c d)")))))

  (run-tests deque-tests))
