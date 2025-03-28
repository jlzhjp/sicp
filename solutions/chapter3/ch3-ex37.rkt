#lang racket/base

(provide c+ c- c* c/ cv)

(require (only-in akari-sicp/lib/constraint-system
                  make-connector
                  multiplier
                  constant
                  adder))

(define (c+ x y)
  (define z (make-connector))
  (adder x y z)
  z)

(define (c- x y)
  (define z (make-connector))
  (adder z y x)
  z)

(define (c* x y)
  (define z (make-connector))
  (multiplier x y z)
  z)

(define (c/ x y)
  (define z (make-connector))
  (multiplier z y x)
  z)

(define (cv x)
  (define c (make-connector))
  (constant x c)
  c)

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))


(module+ test
  (require akari-sicp/lib/testing
           (only-in akari-sicp/lib/constraint-system
                    probe
                    set-value!
                    get-value
                    has-value?))

  (run-tests
   (describe "Celsius to Fahrenheit Converter Tests"
     (it "converts 0 Celsius to 32 Fahrenheit"
       (define c (cv 0))
       (define f (celsius-fahrenheit-converter c))

       (expect [(probe 'f f) =$> '("Probe: f = 32")]
               [(get-value f) => 32]
               [(has-value? f) => #t]))

     (it "converts 100 Celsius to 212 Fahrenheit"
       (define c (cv 100))
       (define f (celsius-fahrenheit-converter c))

       (expect [(probe 'f f) =$> '("Probe: f = 212")]
               [(get-value f) => 212]
               [(has-value? f) => #t])))))