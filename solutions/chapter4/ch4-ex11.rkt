#lang racket/base

(provide evaluator-environment@)

(require racket/unit
         akari-sicp/lib/mcons
         "signatures.rkt")

;; a binding is the variable name with corresponding value
(define (make-binding var val) (mcons var val))
(define (binding-var binding) (mcar binding))
(define (binding-val binding) (mcdr binding))
(define (set-binding-val! binding val) (set-mcdr! binding val))

(define-unit evaluator-environment@
  (import)
  (export evaluator-environment^)

  ;; a frame is a mlist of bindings

  ;; represent an environment as a mlist of frames
  (define (enclosing-environment env) (mcdr env))
  (define (first-frame env) (mcar env))
  (define (add-binding-to-first-frame! var val env)
    (set-mcar! env (mcons (mcons var val) (mcar env))))
  (define the-empty-environment '())

  ;; to extend an environment, we make a frame consisting of the bindings
  ;; and adjoin this frame to the environment
  (define (extend-environment vars vals base-env)
    (define bindings (list->mlist (map make-binding vars vals)))
    (mcons bindings base-env))

  ;; loop up a variable in an environment
  (define (lookup-variable-value var env)
    (define (env-loop env)
      ;; scan the mlist of bindings in the frame
      (define (scan bindings)
        (cond [(null? bindings) ; we have reached the end of current frame
               (env-loop (enclosing-environment env))] ; scan the next frame
              [(eq? var (binding-var (mcar bindings))) ; found the variable
               (binding-val (mcar bindings))] ; return the value
              [else (scan (mcdr bindings))])) ; continue scanning

      (if (eq? env the-empty-environment) ; no more frames
          (error "Unbound variable" var) ; error
          (scan (first-frame env))))
    ;; start the loop
    (env-loop env))

  ;; set a variable to a new value in a specified environment
  (define (set-variable-value! var val env)
    (define (env-loop env)
      (define (scan bindings)
        (cond [(null? bindings) ; the end of a frame
               (env-loop (enclosing-environment env))] ; look up the next frame
              [(eq? var (binding-var (mcar bindings)))
               (set-binding-val! (mcar bindings) val)] ; set the value
              [else (scan (mcdr bindings))]))
      (if (eq? env the-empty-environment)
          (error "Unbound variable -- SET!" var)
          (scan (first-frame env))))
    (env-loop env))

  ;; define a variable
  (define (define-variable! var val env)
    (define (scan bindings)
      ;; if we can't find the variable in the current frame
      (cond [(null? bindings)
             ;; add the variable to the current frame
             (add-binding-to-first-frame! var val env)]
            ;; if we find the variable in the current frame
            [(eq? var (binding-var (mcar bindings)))
             ;; set the value of the variable
             (set-binding-val! (mcar bindings) val)]
            [else (scan (mcdr bindings))])) ; continue scanning
    (scan (first-frame env))))

(module+ test
  (require akari-sicp/lib/testing)

  (define-values/invoke-unit/infer evaluator-environment@)

  (define base-env (mlist (mlist (make-binding 'a 10) (make-binding 'b 20))))

  (run-tests
   (describe "test environment"
     (it "extend-environment"
       (define extended-env (extend-environment (list 'x 'y) (list 1 2) base-env))
       (expect [(mlength extended-env) => 2]
               [(mcar extended-env) => (mlist (make-binding 'x 1) (make-binding 'y 2))]
               [(equal? (msecond extended-env) (mfirst base-env)) => #t]))

     (it "lookup-variable-value"
       (define env (extend-environment (list 'x 'y) (list 1 2) base-env))
       (expect [(lookup-variable-value 'x env) => 1]
               [(lookup-variable-value 'y env) => 2]
               [(lookup-variable-value 'a env) => 10]
               [(lookup-variable-value 'b env) => 20]
               [(lookup-variable-value 'z env) =!> #rx"Unbound variable"]))

     (it "set-variable-value!"
       (define env (extend-environment (list 'x 'y) (list 1 2) base-env))
       (set-variable-value! 'x 100 env)
       (set-variable-value! 'a 1000 env)
       (expect [(lookup-variable-value 'x env) => 100]
               [(lookup-variable-value 'y env) => 2]
               [(lookup-variable-value 'a env) => 1000]
               [(lookup-variable-value 'b env) => 20]
               [(set-variable-value! 'z 30 env) =!> #rx"Unbound variable"]))

     (it "define-variable!"
       (define env (extend-environment (list 'x 'y) (list 1 2) base-env))
       ;; Update existing variable
       (define-variable! 'x 100 env)
       (expect [(lookup-variable-value 'x env) => 100])
       ;; Add new variable to first frame
       (define-variable! 'z 30 env)
       (expect [(lookup-variable-value 'z env) => 30]
               ;; Base environment shouldn't be affected
               [(lookup-variable-value 'z base-env) =!> #rx"Unbound variable"]))) ))