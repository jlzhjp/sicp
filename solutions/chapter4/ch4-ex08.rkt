#lang racket/base

(provide eval-let)

(require racket/match
         (only-in "ch4-ex03.rkt"
                  eval
                  apply
                  current-environment
                  special-form-handlers
                  extend-environment
                  set-variable-value!
                  setup-base-environment
                  make-procedure))

(define ((eval-let eval) datum)
  (match datum
    [(list 'let (? symbol? name) (list (list vars vals) ...) body ...)
     (define lmb
       (parameterize ([current-environment
                       (extend-environment (list name) (list '*)
                                           (current-environment))])
         (define lmb
           (make-procedure
            vars
            body
            (current-environment)))
         (set-variable-value! name lmb)
         lmb))

     (apply lmb
            (let loop ([args vals])
              (if (null? args)
                  '()
                  (cons (eval (car args)) (loop (cdr args))))))]
    [(list 'let (list (list vars vals) ...) body ...)
     (eval `((lambda ,vars ,@body) ,@vals))]))

(module+ test
  (require akari-sicp/lib/testing)
  
  (parameterize ([special-form-handlers
                  (hash-set (special-form-handlers)
                            'let eval-let)])
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
         (parameterize ([current-environment (setup-base-environment)])
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
                   [(eval '(fib 6)) => 8])))))))