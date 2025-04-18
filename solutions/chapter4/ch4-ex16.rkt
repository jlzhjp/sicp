#lang racket/base

(require racket/match
         racket/unit
         "signatures.rkt"
         "ch4-ex03.rkt"
         "ch4-ex08.rkt")

(define (scan-out-defines body)
  (let loop ([exps body] [vars '()] [new-body '()])
    (match exps
      [(cons (list 'define (list var args ...) lambda-body ...) _)
       (loop (cdr exps)
             (cons var vars)
             (cons `(set! ,var (lambda ,args ,@lambda-body)) new-body))]
      [(cons (list 'define var val) _)
       (loop (cdr exps)
             (cons var vars)
             (cons `(set! ,var ,val) new-body))]
      [(cons exp _)
       (loop (cdr exps)
             vars
             (cons exp new-body))]
      ['() (values (reverse vars) (reverse new-body))])))


(define (transform-body body)
  (define-values (vars new-body) (scan-out-defines body))
  (cond [(null? vars) body] ;; important: if no internal definitions, return the original body
        [else
         (define hoisted-definitions (map (lambda (var) `(,var '*unassigned*)) vars))
         `((let ,hoisted-definitions ,@new-body))]))

(define-unit internal-definition-extension@
  (import (prefix base: compound-procedure^)
          (prefix base: evaluator-environment^))
  (export compound-procedure^ evaluator-environment^)

  (define (make-procedure params body env)
    (base:make-procedure params (transform-body body) env))

  (define compound-procedure? base:compound-procedure?)
  (define procedure-parameters base:procedure-parameters)
  (define procedure-body base:procedure-body)
  (define procedure-environment base:procedure-environment)

  (define (lookup-variable-value var env)
    (let ([val (base:lookup-variable-value var env)])
      (if (eq? val '*unassigned*)
          (error 'lookup-variable-value "variable used before bound to its value: ~a" var)
          val)))
  (define the-empty-environment base:the-empty-environment)
  (define extend-environment base:extend-environment)
  (define define-variable! base:define-variable!)
  (define set-variable-value! base:set-variable-value!))

(module+ test
  (require akari-sicp/lib/testing
           akari-sicp/lib/mcons)

  (define-compound-unit internal-definition-handler@
    (import)
    (export cpx eex)
    (link [([cp : compound-procedure^]) compound-procedure@]
          [([ee : evaluator-environment^]) evaluator-environment@]
          [([cpx : compound-procedure^] [eex : evaluator-environment^])
           internal-definition-extension@ cp ee]))

  (define-values/invoke-unit/infer
    (link primitive-procedure@
          internal-definition-handler@
          metacircular-evaluator@
          let-extension@))

  (special-form-handlers (hash-set (special-form-handlers) 'let eval-let))

  (define declaration
    '(define (f x)
       (define (even? n)
         (if (= n 0)
             #t
             (odd? (- n 1))))
       (define (odd? n)
         (if (= n 0)
             #f
             (even? (- n 1))))

       (define x 3)
       (define y 4)

       (cons (even? x) (odd? y))))

  (run-tests
   (describe "exercise 4.16"
     (describe "test `scan-out-defines`"
       (it "shoud extract bindings and body correctly"
         (match-define (list 'define sig body ...) declaration)
         (define-values (bindings new-body) (scan-out-defines body))
         (expect [bindings => (list 'even? 'odd? 'x 'y)]
                 [new-body => (list '(set! even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                                    '(set! odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                                    '(set! x 3)
                                    '(set! y 4)
                                    '(cons (even? x) (odd? y)))])))
     (describe "test `transform-body`"
       (it "should hoist variables correctly"
         (match-define (list 'define sig body ...) declaration)
         (expect [(transform-body body)
                  => '((let ([even? '*unassigned*]
                             [odd? '*unassigned*]
                             [x '*unassigned*]
                             [y '*unassigned*])
                         (set! even? (lambda (n)
                                       (if (= n 0)
                                           #t
                                           (odd? (- n 1)))))
                         (set! odd? (lambda (n)
                                      (if (= n 0)
                                          #f
                                          (even? (- n 1)))))
                         (set! x 3)
                         (set! y 4)
                         (cons (even? x) (odd? y))))]))
       (it "should return original body if no internal definition"
         (define body '((+ x y) (+ y z)))
         (expect [(transform-body body) => body])))
     (describe "evaluate inner definition"
       (it "should evaluate to correct result"
         (eval declaration)
         (expect [(eval '(f 0)) => (mcons #f #f)]))
       (it "should throw error if variable used before declaration"
         (eval '(define (g x)
                  (define y z)
                  (define z x)
                  (cons y z)))
         (expect [(eval '(g 0)) =!> #rx"variable used before bound to its value"]))))))