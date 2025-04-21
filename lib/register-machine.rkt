#lang racket/base

;; (define (make-machine register-names ops controller-text)
;;   (let ([machine (make-new-machine)])
;;     (for-each (lambda (register-name)
;;                 ((machine 'allocate-register) register-name))
;;               register-names)
;;     ((machine 'install-operations) ops)
;;     ((machine 'install-instruction-sequence)
;;      (assemble controller-text machine))
;;     machine))
;; 
;; (define (make-register name)
;;   (let ([contents '*unassigned*])
;;     (define (dispatch message)
;;       (cond [(eq? message 'get) contents]
;;             [(eq? message 'set)
;;              (lambda (value) (set! contents value))]
;;             [else (error 'register "unknown request ~a" message)]))))
;; 
;; (define (get-contents register)
;;   (register 'get))
;; 
;; (define (set-contents! register value)
;;   ((register 'set) value))
;; 
;; (define (make-stack)
;;   (let ([s '()])
;;     (define (push x)
;;       (set! s (cons x s)))
;; 
;;     (define (pop)
;;       (if (null? s)
;;           (error 'pop "empty stack")
;;           (let ([top (car s)])
;;             (set! s (cdr s))
;;             top)))
;; 
;;     (define (initialize)
;;       (set! s '()))
;; 
;;     (define (dispatch message)
;;       (cond [(eq? message 'push) push]
;;             [(eq? message 'pop) (pop)]
;;             [(eq? message 'initialize) (initialize)]
;;             [else (error 'stack "unknown request" message)])))
;;   dispatch)
;; 
;; (define (make-new-machine)
;;   (let ([pc (make-register 'pc)]
;;         [flag (make-register 'flag)]
;;         [stack (make-stack)]
;;         [the-instruction-sequence '()])
;;     (let ([the-ops
;;            (list (list 'initialize-stack
;;                        (lambda () (stack 'initialize))))]
;;           [register-table
;;            (list (list 'pc pc) (list 'flag flag))])
;;       )))