#lang racket/base

(require racket/match)

(define (make-queue)
  (let ([front-ptr '()]
        [rear-ptr '()])

    (define (empty-queue?) (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (mcar front-ptr)))

    (define (insert-queue! item)
      (let ([new-pair (mcons item '())])
        (cond [(empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)]
              [else
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)])))

    (define (delete-queue!)
      (cond [(empty-queue?)
             (error "DELETE! called with an empty queue")]
            [else
             (set! front-ptr (mcdr front-ptr))]))

    (define (traverse-queue body)
      (define (iter node)
        (when (not (null? node))
          (begin (body (mcar node))
                 (iter (mcdr node)))))
      (iter front-ptr))

    (define (dispatch m)
      (match m
        ['empty-queue? empty-queue?]
        ['front-queue front-queue]
        ['insert-queue! (lambda (item) (insert-queue! item) dispatch)]
        ['delete-queue! (lambda () (delete-queue!) dispatch)]
        ['traverse-queue traverse-queue]
        [_ (error "Undefined operation -- DISPATCH" m)]))

    dispatch))

(define (empty-queue? dispatch) ((dispatch 'empty-queue?)))
(define (front-queue dispatch) ((dispatch 'front-queue)))
(define (traverse-queue dispatch body) ((dispatch 'traverse-queue) body))
(define (insert-queue! dispatch item) ((dispatch 'insert-queue!) item))
(define (delete-queue! dispatch) ((dispatch 'delete-queue!)))

(define (print-queue queue)
  (display "(queue")
  (traverse-queue
   queue
   (lambda (v)
     (display " ")
     (display v)))
  (display ")"))


(define q1 (make-queue))

(module+ test
  (require support/testing)

  (check-output
   (lines "(queue a)"
          "(queue a b)"
          "(queue b)"
          "(queue)")
   (print-queue (insert-queue! q1 'a)) (newline)
   (print-queue (insert-queue! q1 'b)) (newline)
   (print-queue (delete-queue! q1)) (newline)
   (print-queue (delete-queue! q1)) (newline)))