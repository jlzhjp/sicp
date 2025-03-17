#lang racket/base

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ([new-pair (mcons item '())])
    (cond [(empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue]
          [else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue])))

(define (delete-queue! queue)
  (cond [(empty-queue? queue)
         (error "DELETE! called with an empty queue" queue)]
        [else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         (when (null? (front-ptr queue))
           (set-rear-ptr! queue '()))
         queue]))

(define (print-queue queue)
  (display "(queue ")

  (define (iter n)
    (if (eq? n (rear-ptr queue))
        (display (mcar n))
        (begin (display (mcar n))
               (display " ")
               (iter (mcdr n)))))
  (when (not (empty-queue? queue))
    (iter (front-ptr queue)))
  (display ")"))

(module+ test
  (require support/testing
           rackunit/text-ui)

  ;; Define comprehensive test suite
  (define queue-tests
    (test-suite
     "Queue Implementation Tests"

     ;; Test 1: New queue is empty
     (test-case "New queue is empty"
                (let ([q (make-queue)])
                  (check-true (empty-queue? q) "New queue should be empty")))

     ;; Test 2: Basic insertion and retrieval
     (test-case "Basic insertion and retrieval"
                (let ([q (make-queue)])
                  (insert-queue! q 'a)
                  (check-equal? (front-queue q) 'a "Front of queue after insertion should be 'a")
                  (check-false (empty-queue? q) "Queue shouldn't be empty after insertion")))

     ;; Test 3: Multiple insertions
     (test-case "Multiple insertions"
                (let ([q (make-queue)])
                  (insert-queue! q 'a)
                  (insert-queue! q 'b)
                  (insert-queue! q 'c)
                  (check-equal? (front-queue q) 'a "Front should be first inserted item")

                  (check-normalized-output
                   (lambda () (print-queue q))
                   '("(queue a b c)"))))

     ;; Test 4: Deletion operations
     (test-case "Deletion operations"
                (let ([q (make-queue)])
                  (insert-queue! q 'a)
                  (insert-queue! q 'b)

                  (delete-queue! q)
                  (check-equal? (front-queue q) 'b "Front should be 'b' after deleting 'a'")

                  (delete-queue! q)
                  (check-true (empty-queue? q) "Queue should be empty after all elements deleted")))

     ;; Test 5: Insert after complete deletion
     (test-case "Insert after complete deletion"
                (let ([q (make-queue)])
                  (insert-queue! q 'a)
                  (delete-queue! q)
                  (check-true (empty-queue? q) "Queue should be empty after deletion")

                  ;; This insertion would fail without the fix to delete-queue!
                  (insert-queue! q 'b)
                  (check-equal? (front-queue q) 'b "Should successfully insert after complete deletion")))

     ;; Test 6: Error conditions
     (test-case "Error conditions"
                (let ([q (make-queue)])
                  (check-exn exn:fail? (lambda () (front-queue q)) "Should throw error on front of empty queue")
                  (check-exn exn:fail? (lambda () (delete-queue! q)) "Should throw error on delete from empty queue")))

     ;; Test 7: Complex sequence of operations
     (test-case "Complex sequence of operations"
                (let ([q (make-queue)])
                  (check-normalized-output
                   (lambda ()
                     (print-queue (insert-queue! q 'a)) (newline)
                     (print-queue (insert-queue! q 'b)) (newline)
                     (print-queue (delete-queue! q)) (newline)
                     (print-queue (insert-queue! q 'c)) (newline)
                     (print-queue (insert-queue! q 'd)) (newline)
                     (print-queue (delete-queue! q)) (newline)
                     (print-queue (delete-queue! q)) (newline)
                     (print-queue (delete-queue! q)) (newline)
                     (print-queue (insert-queue! q 'e)) (newline))
                   '("(queue a)"
                     "(queue a b)"
                     "(queue b)"
                     "(queue b c)"
                     "(queue b c d)"
                     "(queue c d)"
                     "(queue d)"
                     "(queue )"
                     "(queue e)"))))))

    ;; Run the test suite
    (run-tests queue-tests))