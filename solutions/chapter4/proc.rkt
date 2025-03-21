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

(define (tagged-list? x tag) (and (list? x) (eqv? (car x) tag)))

(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? proc) (tagged-list? proc 'procedure))
(define (procedure-parameters proc) (cadr proc))
(define (procedure-body proc) (caddr proc))
(define (procedure-environment proc) (cadddr proc))

(define (make-primitive proc) (list 'primitive proc))
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args) (apply (primitive-implementation proc) args))
