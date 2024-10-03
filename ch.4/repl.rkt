#lang racket/base

(require "proc.rkt")
(require "env.rkt")
(require "eval.rkt")

(define primitive-procedures
  (list (list 'car (make-primitive car))
        (list 'cdr (make-primitive cdr))
        (list 'cons (make-primitive cons))
        (list 'null? (make-primitive null?))))

(define (setup-environment)
  (let* ([primitive-procedure-names (map car primitive-procedures)]
         [primitive-procedure-objects (map cadr primitive-procedures)]
         [initial-env (extend-environment primitive-procedure-names
                                          primitive-procedure-objects
                                          the-empty-environment)])
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define input-prompt ";;; M-Eval input: ")
(define output-prompt ";;; M-Eval value: ")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (let ([output (eval input the-global-environment)])
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(driver-loop)