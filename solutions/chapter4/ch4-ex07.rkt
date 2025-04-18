#lang racket/base

(require racket/match
         racket/unit
         "signatures.rkt"
         "ch4-ex03.rkt"
         "ch4-ex06.rkt")

(define (let*->nested-lets exp)
  (match exp
    [(list 'let* bindings body)
     (let loop ([bds bindings])
       (if (null? bds)
           body
           `(let (,(car bds))
              ,(loop (cdr bds)))))]))

(define-unit let*-extensions@
  (import let-extension^)
  (export let*-extension^)

  (define (eval-let* exp)
    (eval-let (let*->nested-lets exp))))

(module+ test
  (require akari-sicp/lib/testing)

  (define-values/invoke-unit/infer
    (link evaluator-compound@ let-extension@ let*-extensions@))

  (special-form-handlers (hash-set (special-form-handlers) 'let* eval-let*))

  (run-tests
   (describe "test eval-let*"
     (it "expand to nested let"
       (expect [(let*->nested-lets
                 '(let* ([x 1] [y 2])
                    (+ x y)))
                =>
                '(let ([x 1])
                   (let ([y 2])
                     (+ x y)))]))
     (it "eval let* directly"
       (expect [(eval
                 (let* ([x 1] [y (+ x 1)])
                   (+ x y)))
                => 3])))))