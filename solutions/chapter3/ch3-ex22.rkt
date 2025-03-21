#lang racket/base

(provide make-queue
         empty-queue?
         front-queue
         insert-queue!
         delete-queue!
         print-queue
         traverse-queue
         insert-all-queue!
         try-delete-queue!
         clear-queue!
         peek-queue
         size-queue
         queue->list
         queue?)

(require racket/match)

(define (make-queue)
  (let ([front-ptr '()]
        [rear-ptr '()]
        [size 0])

    (define (empty-queue?) (null? front-ptr))

    (define (size-queue) size)

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (mcar front-ptr)))

    (define (peek-queue default)
      (if (empty-queue?)
          default
          (mcar front-ptr)))

    (define (insert-queue! item)
      (let ([new-pair (mcons item '())])
        (cond [(empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)]
              [else
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)])
        (set! size (+ size 1))))

    (define (delete-queue!)
      (cond [(empty-queue?)
             (error "DELETE! called with an empty queue")]
            [else
             (set! front-ptr (mcdr front-ptr))
             (when (null? front-ptr)
               (set! rear-ptr '()))
             (set! size (max 0 (- size 1)))]))

    (define (try-delete-queue! on-empty)
      (if (empty-queue?)
          (on-empty)
          (begin (delete-queue!) #t)))

    (define (clear-queue!)
      (set! front-ptr '())
      (set! rear-ptr '())
      (set! size 0))

    (define (insert-all-queue! items)
      (for-each insert-queue! items))

    (define (traverse-queue body)
      (define (iter node)
        (when (not (null? node))
          (begin (body (mcar node))
                 (iter (mcdr node)))))
      (iter front-ptr))

    (define (queue->list)
      (let ([result '()])
        (traverse-queue
         (lambda (item)
           (set! result (append result (list item)))))
        result))

    (define (dispatch m)
      (match m
        ['empty-queue? empty-queue?]
        ['front-queue front-queue]
        ['peek-queue peek-queue]
        ['size-queue size-queue]
        ['queue->list queue->list]
        ['insert-queue! insert-queue!]
        ['insert-all-queue! insert-all-queue!]
        ['delete-queue! delete-queue!]
        ['try-delete-queue! try-delete-queue!]
        ['clear-queue! clear-queue!]
        ['traverse-queue traverse-queue]
        [_ (error "Undefined operation -- DISPATCH" m)]))

    (list 'queue dispatch)))

(define (empty-queue? dispatch)
  (((cadr dispatch) 'empty-queue?)))

(define (front-queue dispatch)
  (((cadr dispatch) 'front-queue)))

(define (traverse-queue dispatch body)
  (((cadr dispatch) 'traverse-queue) body)
  dispatch)

(define (insert-queue! dispatch item)
  (((cadr dispatch) 'insert-queue!) item)
  dispatch)

(define (delete-queue! dispatch)
  (((cadr dispatch) 'delete-queue!))
  dispatch)

(define (peek-queue dispatch [default #f])
  (((cadr dispatch) 'peek-queue) default))

(define (size-queue dispatch)
  (((cadr dispatch) 'size-queue)))

(define (queue->list dispatch)
  (((cadr dispatch) 'queue->list)))

(define (insert-all-queue! dispatch items)
  (((cadr dispatch) 'insert-all-queue!) items)
  dispatch)

(define (try-delete-queue! dispatch [on-empty (lambda () #f)])
  (((cadr dispatch) 'try-delete-queue!) on-empty))

(define (clear-queue! dispatch)
  (((cadr dispatch) 'clear-queue!))
  dispatch)

(define (print-queue queue)
  (define items (queue->list queue))
  (display "(queue")
  (for-each (lambda (v) (display " ") (display v)) items)
  (display ")"))

(define (queue? x) (eqv? (car x) 'queue))

(module+ test
  (require "../../lib/testing.rkt"
           rackunit/text-ui)

  (define-test-suite queue-tests
    (test-suite
     "Queue Implementation Tests"

     (test-suite
      "Basic queue operations"

      (test-case "Empty queue detection"
                 (let ([q (make-queue)])
                   (check-true (empty-queue? q) "New queue should be empty")))

      (test-case "Insert and verify queue contents"
                 (let ([q (make-queue)])
                   (check-normalized-output
                    (lambda ()
                      (print-queue (insert-queue! q 'a))
                      (newline))
                    '("(queue a)"))

                   (check-normalized-output
                    (lambda ()
                      (print-queue (insert-queue! q 'b))
                      (newline))
                    '("(queue a b)"))

                   (check-equal? (front-queue q) 'a "Front of queue should be 'a'")))

      (test-case "Delete queue elements"
                 (let ([q (make-queue)])
                   (insert-queue! q 'x)
                   (insert-queue! q 'y)

                   (check-normalized-output
                    (lambda ()
                      (print-queue q)
                      (newline)
                      (print-queue (delete-queue! q))
                      (newline))
                    '("(queue x y)" "(queue y)"))

                   (check-equal? (front-queue q) 'y "Front should now be 'y'"))))

     (test-suite
      "Error handling"

      (test-case "Front on empty queue"
                 (let ([q (make-queue)])
                   (check-exn
                    #rx"FRONT called with an empty queue"
                    (lambda () (front-queue q)))))

      (test-case "Delete on empty queue"
                 (let ([q (make-queue)])
                   (check-exn
                    #rx"DELETE! called with an empty queue"
                    (lambda () (delete-queue! q))))))

     (test-suite
      "Queue traversal"

      (test-case "Traverse and compute"
                 (let ([q (make-queue)]
                       [sum 0])
                   (insert-queue! q 1)
                   (insert-queue! q 2)
                   (insert-queue! q 3)

                   (traverse-queue q (lambda (x) (set! sum (+ sum x))))
                   (check-equal? sum 6 "Sum of 1+2+3 should be 6")))

      (test-case "Print queue contents"
                 (let ([q (make-queue)])
                   (insert-queue! q 'a)
                   (insert-queue! q 'b)
                   (insert-queue! q 'c)

                   (check-normalized-output
                    (lambda ()
                      (print-queue q)
                      (newline))
                    '("(queue a b c)")))))

     (test-suite
      "Complex operations"

      (test-case "Chained operations"
                 (let ([q (make-queue)])
                   (insert-queue! q 1)
                   (insert-queue! q 2)

                   (check-normalized-output
                    (lambda ()
                      (print-queue
                       (insert-queue!
                        (delete-queue!
                         (insert-queue! q 3))
                        4))
                      (newline))
                    '("(queue 2 3 4)"))))

      (test-case "Empty and refill queue"
                 (let ([q (make-queue)])
                   (insert-queue! q 'a)
                   (insert-queue! q 'b)
                   (delete-queue! q)
                   (delete-queue! q)
                   (check-true (empty-queue? q) "Queue should be empty after all deletions")

                   (insert-queue! q 'new)
                   (check-false (empty-queue? q) "Queue shouldn't be empty after insertion")
                   (check-equal? (front-queue q) 'new "Front should be the newly inserted item"))))

     (test-suite
      "Data type handling"

      (test-case "Different data types"
                 (let ([q (make-queue)])
                   (insert-queue! q 42)
                   (insert-queue! q 'symbol)
                   (insert-queue! q "string")
                   (insert-queue! q #t)

                   (check-normalized-output
                    (lambda ()
                      (print-queue q)
                      (newline))
                    '("(queue 42 symbol string #t)"))

                   (check-equal? (front-queue q) 42 "Front should be numeric value"))))

     (test-suite
      "Enhanced queue operations"

      (test-suite
       "Queue size tracking"

       (test-case "Empty queue size"
                  (let ([q (make-queue)])
                    (check-equal? (size-queue q) 0 "Empty queue should have size 0")))

       (test-case "Size after insertions"
                  (let ([q (make-queue)])
                    (insert-queue! q 'a)
                    (insert-queue! q 'b)
                    (insert-queue! q 'c)
                    (check-equal? (size-queue q) 3 "Queue should have size 3 after 3 insertions")))

       (test-case "Size after deletions"
                  (let ([q (make-queue)])
                    (insert-queue! q 'a)
                    (insert-queue! q 'b)
                    (delete-queue! q)
                    (check-equal? (size-queue q) 1 "Queue should have size 1 after 1 deletion"))))

      (test-suite
       "Safe queue operations"

       (test-case "Peek on empty queue"
                  (let ([q (make-queue)])
                    (check-equal? (peek-queue q) #f "peek-queue should return #f on empty queue")
                    (check-equal? (peek-queue q 'empty) 'empty "peek-queue should return custom default")))

       (test-case "Peek on non-empty queue"
                  (let ([q (make-queue)])
                    (insert-queue! q 'test)
                    (check-equal? (peek-queue q) 'test "peek-queue should return front item")
                    (check-equal? (front-queue q) 'test "peek should not remove the item")))

       (test-case "Try-delete on empty queue"
                  (let ([q (make-queue)]
                        [was-called #f])
                    (check-false (try-delete-queue! q) "try-delete should return #f for empty queue")
                    (check-equal? (try-delete-queue! q (lambda () 'empty)) 'empty
                                  "try-delete should call on-empty function")
                    (try-delete-queue! q (lambda () (set! was-called #t)))
                    (check-true was-called "on-empty function should be called")))

       (test-case "Try-delete on non-empty queue"
                  (let ([q (make-queue)])
                    (insert-queue! q 'a)
                    (insert-queue! q 'b)
                    (check-true (try-delete-queue! q) "try-delete should return #t on successful deletion")
                    (check-equal? (front-queue q) 'b "front item should be deleted"))))

      (test-suite
       "Batch operations"

       (test-case "Clear queue"
                  (let ([q (make-queue)])
                    (insert-queue! q 'a)
                    (insert-queue! q 'b)
                    (insert-queue! q 'c)
                    (clear-queue! q)
                    (check-true (empty-queue? q) "Queue should be empty after clear")
                    (check-equal? (size-queue q) 0 "Queue size should be 0 after clear")
                    (check-exn #rx"FRONT called with an empty queue"
                               (lambda () (front-queue q)) "Queue should be truly empty")))

       (test-case "Insert all items"
                  (let ([q (make-queue)])
                    (insert-all-queue! q '(1 2 3))
                    (check-equal? (size-queue q) 3 "Queue should have all three elements")
                    (check-equal? (front-queue q) 1 "First element should be at front")

                    (insert-all-queue! q '())
                    (check-equal? (size-queue q) 3 "Empty list should not change queue")

                    (insert-all-queue! q '(4 5))
                    (check-equal? (size-queue q) 5 "Should append new elements")
                    (check-equal? (queue->list q) '(1 2 3 4 5) "Should maintain correct order"))))

      (test-suite
       "Queue to list conversion"

       (test-case "Empty queue to list"
                  (let ([q (make-queue)])
                    (check-equal? (queue->list q) '() "Empty queue should convert to empty list")))

       (test-case "Non-empty queue to list"
                  (let ([q (make-queue)])
                    (insert-queue! q 'a)
                    (insert-queue! q 'b)
                    (insert-queue! q 'c)
                    (check-equal? (queue->list q) '(a b c) "Should convert to list in correct order")
                    ;; Ensure non-destructive
                    (check-equal? (size-queue q) 3 "queue->list should not modify the queue"))))

      (test-suite
       "Complex scenarios with new operations"

       (test-case "Mixed operations sequence"
                  (let ([q (make-queue)])
                    (insert-all-queue! q '(a b c))
                    (check-equal? (peek-queue q) 'a "First element should be 'a'")
                    (delete-queue! q)
                    (check-equal? (size-queue q) 2 "Size should be 2 after deletion")
                    (insert-queue! q 'd)
                    (check-equal? (queue->list q) '(b c d) "Queue should contain b, c, d")
                    (clear-queue! q)
                    (check-true (empty-queue? q) "Queue should be empty after clear")
                    (check-false (try-delete-queue! q) "try-delete on empty queue returns false")
                    (insert-all-queue! q '(x y z))
                    (check-equal? (queue->list q) '(x y z) "Queue should have new elements")))))))

  (run-tests queue-tests))