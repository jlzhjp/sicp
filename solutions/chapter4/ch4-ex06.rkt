#lang racket/base

(provide let-extension@)

(require racket/match
         racket/unit
         "signatures.rkt"
         "ch4-ex03.rkt")

(define (let->combination datum)
  (match datum
    [(list 'let decls body ...)
     (define vars (map car decls))
     (define exps (map cadr decls))
     `((lambda ,vars
         ,@body) ,@exps)]))

;; Handler for let special form
(define-unit let-extension@
  (import metacircular-evaluator^)
  (export let-extension^)
  (define (eval-let datum)
    (eval (let->combination datum))))

(module+ test
  (require akari-sicp/lib/testing)

  (define-values/invoke-unit/infer
    (link evaluator-compound@ let-extension@))

  (special-form-handlers (hash-set (special-form-handlers) 'let eval-let))

  (run-tests
   (describe "let->combination"
     (it "transforms let into a lambda expression"
       (let ((datum '(let ((x 1) (y 2)) (+ x y))))
         (expect [(let->combination datum) => '((lambda (x y) (+ x y)) 1 2)])))

     (describe "let as a special form"
       (it "evaluates let expressions directly"
         (parameterize ([current-environment (extend-environment
                                              '(a b) '(10 20)
                                              (current-environment))])
           (expect [(eval '(let ((x 3) (y 4)) (+ x y))) => 7]
                   [(eval '(let ((x a) (y b)) (+ x y))) => 30])))

       (it "supports nested let expressions"
         (parameterize ([current-environment (extend-environment '(z) '(5) (current-environment))])
           (expect [(eval '(let ((x 1))
                             (let ((y z))
                               (+ x y)))) => 6])))))))