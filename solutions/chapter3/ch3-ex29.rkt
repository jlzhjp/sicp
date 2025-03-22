#lang racket/base

(require racket/contract
         akari-sicp/lib/digital-circuits)

;;       ┌──────────┐ b1
;; a1───►│ inverter ┼─────┐
;;       └──────────┘  ┌──▼───────┐ c ┌────────┐
;;                     │ and-gate ├──►│inverter┼─►output
;;       ┌──────────┐  └──▲───────┘   └────────┘
;; a2───►│ inverter ┼─────┘
;;       └──────────┘ b2

;; or-gate-delay = (+ and-gate-delay (* 2 inverter-delay))

(define/contract (or-gate a1 a2 output)
  (-> wire? wire? wire? void?)
  (define b1 (make-wire))
  (define b2 (make-wire))
  (define c (make-wire))
  (inverter a1 b1)
  (inverter a2 b2)
  (and-gate b1 b2 c)
  (inverter c output))


(module+ test
  (require akari-sicp/lib/testing)

  (run-tests
    (let ([a1 (make-wire)]
          [a2 (make-wire)]
          [output (make-wire)])
      (describe "test or gate"
        #:before (lambda () (or-gate a1 a2 output))

        (it "0 or 0 should be 0"
          (set-signal! a1 0)
          (set-signal! a2 0)
          (propagate)
          (expect [(get-signal output) => 0]))

        (it "0 or 1 should be 1"
          (set-signal! a1 0)
          (set-signal! a2 1)
          (propagate)
          (expect [(get-signal output) => 1]))

        (it "1 or 0 should be 1"
          (set-signal! a1 1)
          (set-signal! a2 0)
          (propagate)
          (expect [(get-signal output) => 1]))

        (it "1 or 1 should be 1"
          (set-signal! a1 1)
          (set-signal! a2 1)
          (propagate)
          (expect [(get-signal output) => 1]))))))