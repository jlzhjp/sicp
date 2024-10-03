#lang racket/base

(provide make-procedure
         compound-procedure?
         procedure-parameters
         procedure-body
         procedure-environment
         make-primitive
         primitive-procedure?
         primitive-implementation
         apply-primitive-procedure)

(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? p) (eqv? 'procedure (car p)))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (make-primitive proc) (list 'primitive proc))
(define (primitive-procedure? proc) (eqv? (car proc) 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args) (apply (primitive-implementation proc) args))
