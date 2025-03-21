#lang racket/base

(require racket/contract
         akari-sicp/lib/digital-circuits
         (only-in "ch3-ex28.rkt" or-gate))

(define/contract (half-adder a b s c)
  (-> wire? wire? wire? wire? void?)
  (define d (make-wire))
  (define e (make-wire))
  (or-gate a b e)
  (and-gate a b c)
  (inverter c e)
  (and-gate d e s))

(define/contract (full-adder a b c-in sum c-out)
  (-> wire? wire? wire? wire? wire? void?)
  (define s (make-wire))
  (define c1 (make-wire))
  (define c2 (make-wire))
  (half-adder b c-in s c1)
  (half-adder a s sum c2)
  (or-gate c1 c2 c-out))
