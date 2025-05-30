#lang racket/base

(provide scale-stream)

(require akari-sicp/lib/stream
         (only-in "ch3-ex50.rkt" stream-map))

; Exercise 3.56: A famous problem, first raised by R. Hamming,
; is to enumerate, in ascending order with no repetitions,
; all positive integers with no prime factors other than
; 2, 3, or 5. One obvious way to do this is to simply test each
; integer in turn to see whether it has any factors other than
; 2, 3, and 5. But this is very inefficient, since, as the integers
; get larger, fewer and fewer of them fit the requirement. As
; an alternative, let us call the required stream of numbers S
; and notice the following facts about it.
; - S begins with 1.
; - The elements of (scale-stream S 2) are also elements of S.
; - The same is true for (scale-stream S 3) and (scale-stream 5 S)
; - These are all the elements of S.

(define (merge-stream s1 s2)
  (cond [(stream-null? s1) s2]
        [(stream-null? s2) s1]
        [else
         (let ([s1car (stream-car s1)]
               [s2car (stream-car s2)])
           (cond [(< s1car s2car)
                  (cons-stream s1car (merge-stream (stream-cdr s1) s2))]
                 [(> s1car s2car)
                  (cons-stream s2car (merge-stream s1 (stream-cdr s2)))]
                 [else
                  (cons-stream s1car (merge-stream (stream-cdr s1) (stream-cdr s2)))]))]))


(define (scale-stream s f)
  (stream-map (lambda (x) (* x f)) s))

(define S
  (cons-stream 1
               (merge-stream (scale-stream S 2)
                             (merge-stream (scale-stream S 3)
                                           (scale-stream S 5)))))

(module+ test
  (require akari-sicp/lib/testing)
  (check-stream-prefix-= S '(1 2 3 4 5 6 8 9 10 12 15)))
