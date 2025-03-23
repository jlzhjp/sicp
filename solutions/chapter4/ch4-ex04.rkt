#lang racket/base

(provide eval-and
         eval-or)

(require racket/match)

(define (expand-and-expressions exps)
  (if (null? exps)
      '#t
      `(if ,(car exps)
           ,(expand-and-expressions (cdr exps))
           #f)))

(define ((eval-and eval) datum)
  (match datum
    [(list 'and exps ...)
     (eval (expand-and-expressions exps))]))

(define (expand-or-expressions exps)
  (if (null? exps)
      '#f
      `(if ,(car exps)
           #t
           ,(expand-or-expressions (cdr exps)))))

(define ((eval-or eval) datum)
  (match datum
    [(list 'or exps ...)
     (eval (expand-or-expressions exps))]))

(module+ test
  (require akari-sicp/lib/testing)

  (run-tests
   (describe "Logical Expression Expansion Tests"

     (describe "expand-and-expressions"
       (it "should expand empty and expression to #t"
         (expect [(expand-and-expressions '()) => '#t]))

       (it "should correctly expand and with a single expression"
         (expect [(expand-and-expressions '(a)) => '(if a #t #f)]))

       (it "should correctly expand and with multiple expressions"
         (expect [(expand-and-expressions '(a b c)) => '(if a (if b (if c #t #f) #f) #f)])))

     (describe "expand-or-expressions"
       (it "should expand empty or expression to #f"
         (expect [(expand-or-expressions '()) => '#f]))

       (it "should correctly expand or with a single expression"
         (expect [(expand-or-expressions '(a)) => '(if a #t #f)]))

       (it "should correctly expand or with multiple expressions"
         (expect [(expand-or-expressions '(a b c)) => '(if a #t (if b #t (if c #t #f)))])))

     ;; Test the evaluator functions with mock eval function
     (describe "eval-and function"
       (it "should use the expanded form for evaluation"
         (let ([mock-eval (lambda (expr)
                            (expect [expr => '(if a (if b #t #f) #f)])
                            'result)])
           (expect [((eval-and mock-eval) '(and a b)) => 'result]))))

     (describe "eval-or function"
       (it "should use the expanded form for evaluation"
         (let ([mock-eval (lambda (expr)
                            (expect [expr => '(if a #t (if b #t #f))])
                            'result)])
           (expect [((eval-or mock-eval) '(or a b)) => 'result])))))))
