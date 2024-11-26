#lang racket/base

(provide cons-stream
         the-empty-stream
         stream-null?
         stream-car
         stream-cdr
         stream-exact-=
         stream-prefix-=
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

(define (stream-exact-= stream lst)
  (cond [(and (null? lst) (stream-null? stream)) #t]
        [(or (null? lst) (stream-null? stream)) #f]
        [(= (stream-car stream) (car lst))
         (stream-prefix-= (stream-cdr stream) (cdr lst))]
        [else #f]))

(define (stream-prefix-= stream lst)
  (cond [(null? lst) #t]
        [(stream-null? stream) #f]
        [(= (stream-car stream) (car lst))
         (stream-prefix-= (stream-cdr stream) (cdr lst))]
        [else #f]))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
