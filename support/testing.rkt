#lang racket/base

(provide stream-prefix-=
         stream-exact-=
         check-stream-prefix-=
         check-stream-exact-=
         (all-from-out rackunit))

(require "stream.rkt"
         rackunit)

(define (stream-prefix-= stream lst)
  (cond [(null? lst) #t]
        [(stream-null? stream) #f]
        [(= (stream-car stream) (car lst))
         (stream-prefix-= (stream-cdr stream) (cdr lst))]
        [else #f]))

(define (stream-exact-= stream lst)
  (cond [(and (null? lst) (stream-null? stream)) #t]
        [(or (null? lst) (stream-null? stream)) #f]
        [(= (stream-car stream) (car lst))
         (stream-prefix-= (stream-cdr stream) (cdr lst))]
        [else #f]))

(define-check (check-stream-prefix-= stream lst)
  (check-true (stream-prefix-= stream lst)))

(define-check (check-stream-exact-= stream lst)
  (check-true (stream-exact-= stream lst)))
