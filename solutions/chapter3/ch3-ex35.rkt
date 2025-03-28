#lang racket/base

(require racket/match
         akari-sicp/lib/constraint-system)

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 ~a" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (when (has-value? a)
          (set-value! b (* (get-value a) (get-value a)) me))))

  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))

  (define me
    (match-lambda
      ['I-have-a-value (process-new-value)]
      ['I-lost-my-value (process-forget-value)]
      [otherwise (error 'squarer "unknown request ~a" otherwise)]))

  (connect a me)
  (connect b me)
  me)

(module+ test
  (require akari-sicp/lib/testing)

  (run-tests
   (describe "Squarer Tests"
     (it "propagates from a to b"
       (define a (make-connector))
       (define b (make-connector))
       (squarer a b)
       (probe "b" b)
       (expect [(set-value! a 10 'user) =$> '("Probe: b = 100")]
               [(get-value b) => 100]
               [(has-value? b) => #t]))

     (it "propagates from b to a"
       (define a (make-connector))
       (define b (make-connector))
       (squarer a b)
       (probe "a" a)
       (expect [(set-value! b 25 'user) =$> '("Probe: a = 5")]
               [(has-value? a) => #t]
               [(get-value a) => 5])))))
