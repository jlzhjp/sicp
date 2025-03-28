#lang racket/base

(require (only-in akari-sicp/lib/constraint-system
                  make-connector
                  multiplier
                  constant
                  adder))

(define (squarer a b)
  (multiplier a a b))

(module+ test
  (require akari-sicp/lib/testing
           (only-in akari-sicp/lib/constraint-system
                    probe
                    set-value!
                    get-value
                    has-value?))

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
       (set-value! b 25 'user)
       ;; read the logic of multiplier
       ;; when b is set, the a wait itself to be set
       ;; so it will not propagate
       (expect [(get-value a) => #f]
               [(has-value? a) => #f])))))