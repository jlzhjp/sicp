#lang racket/base
; (require "env.rkt")

; (define env (mcons (make-frame '() '()) '()))
; (define-variable! 'x 10 env)
; (define extended-env (extend-environment (mcons 'y '()) (mcons 100 '()) env))
; (display (lookup-variable-value 'x extended-env)) (newline)
; (display (lookup-variable-value 'y extended-env)) (newline)

(define (main) (test))

(define (test) (display "Hello"))
