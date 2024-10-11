#lang racket/base

(require rackunit)

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (= kinds-of-coins 0)) 0]
        [else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins))]))

(define (trace-call from amount kinds-of-coins)
  (displayln (format "\"(cc ~a ~a)\" -- \"(cc ~a ~a)\""
                     (car from)
                     (cadr from)
                     amount
                     kinds-of-coins))
  (cc-trace amount kinds-of-coins))

(define (trace-result from result)
  (displayln (format "\"(cc ~a ~a)\" -- \"~a\""
                     (car from)
                     (cadr from)
                     result))
  result)

(define (cc-trace amount kinds-of-coins)
  (define args (list amount kinds-of-coins))
  (cond [(= amount 0) (trace-result args 1)]
        [(or (< amount 0) (= kinds-of-coins 0)) (trace-result args 0)]
        [else (+ (trace-call args
                             amount
                             (- kinds-of-coins 1))
                 (trace-call args
                             (- amount
                                (first-denomination
                                 kinds-of-coins))
                             kinds-of-coins))]))

(define (first-denomination kinds-of-coins)
  (cond [(= kinds-of-coins 1) 1]
        [(= kinds-of-coins 2) 5]
        [(= kinds-of-coins 3) 10]
        [(= kinds-of-coins 4) 25]
        [(= kinds-of-coins 5) 50]))

(check-= (count-change 11) 4 0)
; (check-= (cc-trace 11 5) 4 0)