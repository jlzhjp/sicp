#lang racket/base

(provide or-gate)

(require racket/match
         racket/contract
         akari-sicp/lib/digital-circuits)

(define or-gate-delay 5)

(define/match (logical-or _a _b)
  [(0 0) 0]
  [(0 1) 1]
  [(1 0) 1]
  [(1 1) 1]
  [(_ _) (error 'logical-or "Invalid signal value")])


(define/contract (or-gate a1 a2 output)
  (-> wire? wire? wire? void?)
  (define (or-action-procedure)
    (define new-value (logical-or (get-signal a1) (get-signal a2)))
    (after-delay or-gate-delay
                 (lambda ()
                   (set-signal! output new-value))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure))


(module+ test
  (require akari-sicp/lib/testing)

  (define a1 (make-wire))
  (define a2 (make-wire))
  (define output (make-wire))

  (define or-gate-tests
    (describe "test or gate"
      #:before (lambda () (or-gate a1 a2 output))

      (it "0 or 0 = 0"
        (set-signal! a1 0)
        (set-signal! a2 0)
        (propagate)
        (expect [(get-signal output) => 0]))

      (it "0 or 1 = 1"
        (set-signal! a1 0)
        (set-signal! a2 1)
        (propagate)
        (expect [(get-signal output) => 1]))

      (it "1 or 0 = 1"
        (set-signal! a1 1)
        (set-signal! a2 0)
        (propagate)
        (expect [(get-signal output) => 1]))

      (it "1 or 1 = 1"
        (set-signal! a1 1)
        (set-signal! a2 1)
        (propagate)
        (expect [(get-signal output) => 1])))))


(module+ test
  (require akari-sicp/lib/testing
           rackunit/text-ui)
  (run-tests or-gate-tests))