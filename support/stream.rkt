#lang racket/base

(provide cons-stream
         the-empty-stream
         stream-null?
         stream-car
         stream-cdr
         stream-ref
         stream-enumerate-interval
         delay
         force)

(require racket/promise)

(define-syntax cons-stream
  (syntax-rules ()
    [(_ A B) (cons A (delay B))]))

(define stream-null? null?)

(define the-empty-stream '())

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
