#lang racket/base

(provide make-frame
         the-empty-environment
         extend-environment
         lookup-variable-value
         set-variable-value!
         define-variable!)

(require compatibility/mlist)

; 将环境表示为一个框架的表
(define (enclosing-environment env) (mcdr env))
(define (first-frame env) (mcar env))
(define the-empty-environment '())

; 每个框架都是一对表形成的序对：1. 这一框架中的所有变量的表；2. 它们的约束的表
(define (make-frame variables values)
  (mcons variables values))

(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (let ([mvars (list->mlist vars)] [mvals (list->mlist vals)])
        (mcons (make-frame mvars mvals) base-env))
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (mcar vars))
             (mcar vals)]
            [else (scan (mcdr vars) (mcdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond [(null? vars)
             (env-loop (enclosing-environment env))]
            [(eq? var (mcar vars)) (set-mcar! vals val)]
            [else (scan (mcdr vars) (mcdr vals))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ([frame (first-frame env)])
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (define (scan vars vals)
      (cond [(null? vars)
             (add-binding-to-frame! var val frame)]
            [(eq? var (mcar vars))
             (set-mcar! vals val)]
            [else (scan (mcdr vars) (mcdr vals))]))
    (scan (frame-variables frame)
          (frame-values frame))))