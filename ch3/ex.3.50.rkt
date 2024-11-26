#lang racket/base

(require "../stream.rkt")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(module+ test
  (require rackunit)

  (check-true
   (stream-exact-=
    (stream-map +
                (stream-enumerate-interval 1 5)
                (stream-enumerate-interval 11 15))
    (list 12 14 16 18 20))))