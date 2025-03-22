#lang racket/base

(provide make-segment
         start-segment
         end-segment)

(define (make-segment start end)
  (list start end))

(define start-segment car)
(define end-segment cadr)


(module+ test
  (require akari-sicp/lib/testing)
  (require (only-in "ch2-ex46.rkt" make-vect))

  (define v1 (make-vect 1 2))
  (define v2 (make-vect 3 4))
  (define v3 (make-vect 5 6))
  (define v4 (make-vect 7 8))

  (define seg1 (make-segment v1 v2))
  (define seg2 (make-segment v3 v4))

  (define segment-tests
    (describe "segment implementation (exercise 2.48)"
      (it "creates a segment and accesses components"
        (expect
         [(start-segment seg1) => v1]
         [(end-segment seg1) => v2]))

      (it "works with different vectors"
        (expect
         [(start-segment seg2) => v3]
         [(end-segment seg2) => v4]))))

  (run-tests segment-tests))
