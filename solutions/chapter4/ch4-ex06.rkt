#lang racket/base

(provide let->combination)

(require racket/match
         (only-in "ch4-ex03.rkt" eval current-environment special-form-handlers extend-environment))

(define (let->combination datum)
  (match datum
    [(list 'let decls body ...)
     (define vars (map car decls))
     (define exps (map cadr decls))
     `((lambda ,vars
         ,@body) ,@exps)]))

;; Handler for let special form
(define ((eval-let eval) exp)
  (eval (let->combination exp)))

(module+ test
  (require akari-sicp/lib/testing)

  (run-tests
   (describe "let->combination"
     (it "transforms let into a lambda expression"
       (let ((datum '(let ((x 1) (y 2)) (+ x y))))
         (expect [(let->combination datum) => '((lambda (x y) (+ x y)) 1 2)])))

     (describe "let as a special form"
       (it "evaluates let expressions directly"
         (parameterize ([special-form-handlers (hash-set (special-form-handlers) 'let eval-let)]
                        [current-environment (extend-environment '(a b) '(10 20) (current-environment))])
           (expect [(eval '(let ((x 3) (y 4)) (+ x y))) => 7]
                   [(eval '(let ((x a) (y b)) (+ x y))) => 30])))

       (it "supports nested let expressions"
         (parameterize ([special-form-handlers (hash-set (special-form-handlers) 'let eval-let)]
                        [current-environment (extend-environment '(z) '(5) (current-environment))])
           (expect [(eval '(let ((x 1))
                             (let ((y z))
                               (+ x y)))) => 6])))

       (it "works with more complex expressions"
         (parameterize ([special-form-handlers (hash-set (special-form-handlers) 'let eval-let)]
                        [current-environment (extend-environment '(base multiplier) '(2 3) (current-environment))])
           (expect [(eval '(let ((a base) (b multiplier)) (* a b))) => 6])))))))