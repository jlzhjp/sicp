#lang racket/base

(require racket/match
         (only-in "ch4-ex06.rkt" eval-let))

(define (let*->nested-lets exp)
  (match exp
    [(list 'let* bindings body)
     (let loop ([bds bindings])
       (if (null? bds)
           body
           `(let (,(car bds))
              ,(loop (cdr bds)))))]))

(define ((eval-let* eval) exp)
  ((eval-let eval) (let*->nested-lets exp)))

(module+ test
  (require akari-sicp/lib/testing
           "ch4-ex03.rkt") ;; eval
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
       (parameterize
           ([special-form-handlers
             (hash-set (special-form-handlers)
                       'let* eval-let*)])
         (expect [(eval
                   (let* ([x 1] [y (+ x 1)])
                     (+ x y)))
                  => 3]))))))