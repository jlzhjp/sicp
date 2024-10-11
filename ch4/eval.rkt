#lang racket/base

(provide eval)

(require "env.rkt")
(require "expand.rkt")
(require "proc.rkt")

(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))

(define (text-of-qutation exp env) (cadr exp))

; (lambda (<parameter_1> ... <parameter_n>) <body>)
(define (eval-lambda exp env)
  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))

(define (eval-if exp env)
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exp env)
  (define (last-exp? seq) (null? (cdr seq)))
  (define (first-exp seq) (car seq))
  (define (rest-exps seq) (cdr seq))
  (let ([exps (cdr exp)])
    (cond [(last-exp? exps) (eval (first-exp exps) env)]
          [else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env)])
    'ok))

; (begin <sequence>)
(define (eval-begin exp env)
  (define (begin-actions exp) (cdr exp))
  (eval-sequence (begin-actions exp) env))

; (set! <variable> <value>)
(define (eval-assignment exp env)
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

; (define <var> <value>)
; (define (<var> <parameter_1> ... <parameter_n>)
;   <body>)
; (define <var>
;   (lambda (<parameter_1> ... <parameter_n>))
(define (eval-definition exp env)
  (define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))

  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))

  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (eval-cond exp env)
  (eval (cond->if exp) env))

(define (eval-let exp env)
  (eval (let->combination exp) env))

(define eval-table
  (make-hash
   (list (cons 'quote text-of-qutation)
         (cons 'lambda eval-lambda)
         (cons 'if eval-if)
         (cons 'begin eval-begin)
         (cons 'set! eval-assignment)
         (cons 'define eval-definition)
         (cons 'cond eval-cond)
         (cons 'let eval-let))))

(define (eval exp env)
  (define (self-evaluating? exp)
    (or (number? exp) (string? exp)))
  (define (variable? exp) (symbol? exp))
  (define (application? exp) (pair? exp))
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))

  (define (list-of-values exps env)
    (define (no-operands? ops) (null? ops))
    (define (first-operand ops) (car ops))
    (define (rest-operands ops) (cdr ops))
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))

  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(hash-ref eval-table (car exp) #f)
         ((hash-ref eval-table (car exp)) exp env)]
        [(application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env))]
        [else
         (error "Unknown expression type -- EVAL" exp)]))

; apply
(define (apply procedure arguments)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments)]
        [(compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure)))]
        [else
         (error
          "Unknown procedure type: APPLY" procedure)]))
