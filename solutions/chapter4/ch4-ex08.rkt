#lang racket/base

(provide let-extension@)

(require racket/match
         racket/unit
         "signatures.rkt"
         "ch4-ex03.rkt")

(define-unit let-extension@
  (import metacircular-evaluator^
          evaluator-environment^
          compound-procedure^)
  (export let-extension^)

  (define (eval-let datum)
    (match datum
      [(list 'let (? symbol? name) (list (list vars vals) ...) body ...)
       (define lmb
         (parameterize ([current-environment
                         (extend-environment (list name) (list '**unassigned**)
                                             (current-environment))])
           (define lmb
             (make-procedure
              vars
              body
              (current-environment)))
           (set-variable-value! name lmb (current-environment))
           lmb))

       (apply lmb
              (let loop ([args vals])
                (if (null? args)
                    '()
                    (cons (eval (car args)) (loop (cdr args))))))]
      [(list 'let (list (list vars vals) ...) body ...)
       (eval `((lambda ,vars ,@body) ,@vals))]
      [_ (error 'eval-let "invalid let form ~a" datum)]))
  )

(module+ test
  (require akari-sicp/lib/testing)

  (define-values/invoke-unit/infer
    (link evaluator-compound@ let-extension@))

  (special-form-handlers (hash-set (special-form-handlers) 'let eval-let))

  (run-tests
   (describe "eval-let with named let support"
     (it "evaluates let expressions directly"
       (parameterize
           ([current-environment
             (extend-environment '(a b) '(10 20) (current-environment))])
         (expect [(eval '(let ((x 3) (y 4)) (+ x y))) => 7]
                 [(eval '(let ((x a) (y b)) (+ x y))) => 30])))

     (it "supports nested let expressions"
       (parameterize
           ([current-environment
             (extend-environment '(z) '(5) (current-environment))])
         (expect [(eval '(let ((x 1))
                           (let ((y z))
                             (+ x y)))) => 6])))

     (it "support named let"
       (eval
        '(define (fib n)
           (let fib-iter ([a 1] [b 0] [count n])
             (if (= count 0)
                 b
                 (fib-iter (+ a b) a (- count 1))))))
       (expect [(eval '(fib 1)) => 1]
               [(eval '(fib 2)) => 1]
               [(eval '(fib 3)) => 2]
               [(eval '(fib 4)) => 3]
               [(eval '(fib 5)) => 5]
               [(eval '(fib 6)) => 8])))))