#lang racket/base

(provide or-gate)

(require racket/match
         racket/contract
         sicp-lib/digital-circuits)

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
