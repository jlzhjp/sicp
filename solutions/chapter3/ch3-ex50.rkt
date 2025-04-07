#lang racket/base

(provide stream-map)

(require akari-sicp/lib/stream)

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(module+ test
  (require akari-sicp/lib/testing)

  (run-tests
   (describe "test stream-map"
     (it "should work properly"
       (define sum-stream
         (stream-map +
                     (stream-enumerate-interval 1 5)
                     (stream-enumerate-interval 11 15)))
       (expect [sum-stream =>> (list 12 14 16 18 20)])))))