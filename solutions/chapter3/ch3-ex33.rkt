#lang racket/base

(require (only-in akari-sicp/lib/constraint-system
                  make-connector
                  multiplier
                  constant
                  adder))

(define (averager a b c)
  (define s (make-connector))
  (adder a b s)

  (define _2 (make-connector))
  (constant 2 _2)

  (define p (make-connector))
  (multiplier _2 c s))

(module+ test
  (require akari-sicp/lib/testing
           (only-in akari-sicp/lib/constraint-system
                    probe
                    set-value!
                    get-value
                    has-value?))

  (run-tests
   (describe "Averager Tests"
     (it "propagates from a and b to c"
       (define a (make-connector))
       (define b (make-connector))
       (define c (make-connector))
       (averager a b c)
       (probe "c" c)
       (set-value! a 10 'user)
       (expect [(set-value! b 20 'user) =$> '("Probe: c = 15")]
               [(get-value c) => 15]
               [(has-value? c) => #t]))

     (it "propagates from a and c to b"
       (define a (make-connector))
       (define b (make-connector))
       (define c (make-connector))
       (averager a b c)
       (probe "b" b)
       (set-value! a 8 'user)
       (expect [(set-value! c 13 'user) =$> '("Probe: b = 18")]
               [(get-value b) => 18]
               [(has-value? b) => #t]))

     (it "propagates from b and c to a"
       (define a (make-connector))
       (define b (make-connector))
       (define c (make-connector))
       (averager a b c)
       (probe "a" a)
       (set-value! b 50 'user)
       (expect [(set-value! c 35 'user) =$> '("Probe: a = 20")]
               [(get-value a) => 20]
               [(has-value? a) => #t]))

     (it "raises error on contradiction"
       (define a (make-connector))
       (define b (make-connector))
       (define c (make-connector))
       (averager a b c)
       (set-value! a 10 'user)
       (set-value! b 20 'user)
       (expect [(set-value! c 20 'user) =!> #rx"contradiction"])))))
