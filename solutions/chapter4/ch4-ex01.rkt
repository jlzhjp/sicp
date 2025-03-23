#lang racket/base

(define ((list-of-values-ltr eval) exps)
  (if (null? exps)
      '()
      (let ([first-value (eval (car exps))])
        (cons first-value
              ((list-of-values-ltr eval) (cdr exps))))))

(define ((list-of-values-rtl eval) exps)
  (if (null? exps)
      '()
      (let ([rest-values ((list-of-values-rtl eval) (cdr exps))])
        (cons (eval (car exps)) rest-values))))

(module+ test
  (require akari-sicp/lib/testing)

  (define eval-sequence '())

  ;; Mock eval function that records evaluation order
  (define (mock-eval exp)
    (set! eval-sequence (append eval-sequence (list exp)))
    exp)

  ;; Create test list-of-values functions with our mock eval
  (define list-of-values-ltr-test (list-of-values-ltr mock-eval))
  (define list-of-values-rtl-test (list-of-values-rtl mock-eval))
  ;; Test expressions
  (define test-expressions '(1 2 3 4 5))

  ;; Test suite
  (run-tests
   (describe "Exercise 4.1: list-of-values evaluation order"
     (it "evaluates expressions from left to right with list-of-values-ltr"
       (set! eval-sequence '())
       (list-of-values-ltr-test test-expressions)
       (expect [eval-sequence => '(1 2 3 4 5)]))

     (it "evaluates expressions from right to left with list-of-values-rtl"
       (set! eval-sequence '())
       (list-of-values-rtl-test test-expressions)
       (expect [eval-sequence =>  '(5 4 3 2 1)]))

     (it "returns the correct results regardless of evaluation order (ltr)"
       (expect [(list-of-values-ltr-test test-expressions) => '(1 2 3 4 5)]))

     (it "returns the correct results regardless of evaluation order (rtl)"
       (expect [(list-of-values-rtl-test test-expressions) => '(1 2 3 4 5)]))

     (it "handles empty expressions list for ltr"
       (expect [(list-of-values-ltr-test '()) => '()]))

     (it "handles empty expressions list for rtl"
       (expect [(list-of-values-rtl-test '()) => '()])))))