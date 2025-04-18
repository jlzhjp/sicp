#lang racket/base

(require racket/match
         racket/unit
         "signatures.rkt"
         "ch4-ex03.rkt")

(define-signature while-extension^
  (eval-while))

(define-unit while-extension@
  (import metacircular-evaluator^)
  (export while-extension^)

  (define (eval-while datum)
    (define while-sym (gensym "while-loop"))
    (match datum
      [(list 'while predicate body ...)
       (eval
        `(begin
           (define (,while-sym)
             (if ,predicate
                 (begin ,@body (,while-sym))
                 'done))
           (,while-sym)))])))

(module+ test
  (require akari-sicp/lib/testing)

  (define-values/invoke-unit/infer
    (link evaluator-compound@ while-extension@))

  (special-form-handlers (hash-set (special-form-handlers) 'while eval-while))

  (run-tests
   (describe "test while loop"
     (it "exit immediately"
       (expect [(eval '(while #f (displayln "X"))) =$> '()]))

     (it "calculate sum"
       (define exp
         '(begin
            (define s 0)
            (define i 1)
            (while (< i 10)
              (set! s (+ s i))
              (set! i (+ i 1)))
            (cons s i)))

       (expect [(eval exp) => (mcons 45 10)])))))
