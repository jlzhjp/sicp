#lang racket/base


(provide primitive-procedure^
         compound-procedure^
         evaluator-environment^
         metacircular-evaluator^
         cond-extension^
         let-extension^
         let*-extension^)

(require racket/unit)

(define-signature primitive-procedure^
  (make-primitive
   primitive-procedure?
   primitive-implementation
   apply-primitive-procedure))

(define-signature compound-procedure^
  (make-procedure
   compound-procedure?
   procedure-parameters
   procedure-body
   procedure-environment))

(define-signature evaluator-environment^
  (the-empty-environment
   extend-environment
   define-variable!
   set-variable-value!
   lookup-variable-value))

(define-signature metacircular-evaluator^
  (eval
   apply
   current-environment
   special-form-handlers))

(define-signature cond-extension^
  (eval-cond))

(define-signature let-extension^
  (eval-let))

(define-signature let*-extension^
  (eval-let*))