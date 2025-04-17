#lang racket/base

(provide the-empty-environment
         extend-environment
         lookup-variable-value
         set-variable-value!
         define-variable!)

(require akari-sicp/lib/mcons)

;; each frame of an environment is represented as a pair of lists:
;; - a list of the variables bound in the frame
;; - a list of the associated values
(define (make-frame variables values)
  (mcons variables values))

(define (frame-variables frame) (mcar frame))

(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))


(module+ test
  (require akari-sicp/lib/testing
           racket/list)

  (define test-frame (make-frame (mlist 'x 'y) (mlist 1 2)))

  (run-tests
   (describe "test frame"
     (it "frame-variables / frame-values"
       (expect [(frame-variables test-frame) => (mlist 'x 'y)]
               [(frame-values test-frame) => (mlist 1 2)]))
     (it "add-binding-to-frame!"
       (add-binding-to-frame! 'z 3 test-frame)
       (expect [(frame-variables test-frame) => (mlist 'z 'x 'y)]
               [(frame-values test-frame) => (mlist 3 1 2)])))))


;; represent an environment as a list of frames
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;; to extend an environment, we make a frame consisting of
;; the list of variables and the list of values
;; and we adjoin this frame to the environment
(define (extend-environment vars vals base-env)
  (when (< (length vars) (length vals))
    (error "Too many arguments supplied" vars vals))
  (when (> (length vars) (length vals))
    (error "Too few arguments supplied" vars vals))

  (cons (make-frame (list->mlist vars) (list->mlist vals)) base-env))

(define (find-in-environment env var found not-found)
  (define (scan vars vals)
    (cond [(null? vars)
           (not-found (first-frame env) (enclosing-environment env))]
          [(eq? var (mcar vars))
           (found vals)]
          [else (scan (mcdr vars) (mcdr vals))]))
  (if (eq? env the-empty-environment)
      (error 'find-in-environment "Unbound variable ~v" var)
      (let ([frame (first-frame env)])
        (scan (frame-variables frame) (frame-values frame)))))

;; loop up a variable in an environment
(define (lookup-variable-value var env)
  (find-in-environment
   env
   var
   (lambda (vals) (mcar vals))
   (lambda (_ enclosing) (lookup-variable-value var enclosing))))

;; set a variable to a new value in a specified environment
(define (set-variable-value! var val env)
  (find-in-environment
   env
   var
   (lambda (vals) (set-mcar! vals val))
   (lambda (_ enclosing) (set-variable-value! var val enclosing))))

(define (define-variable! var val env)
  (find-in-environment
   env
   var
   (lambda (vals) (set-mcar! vals val))
   (lambda (frame _) (add-binding-to-frame! var val frame))))

(module+ test
  (define base-env (list (make-frame (mlist 'a 'b) (mlist 10 20))))

  (run-tests
   (describe "test environment"
     (it "extend-environment"
       (define extended-env (extend-environment (list 'x 'y) (list 1 2) base-env))
       (expect [(length extended-env) => 2]
               [(frame-variables (first-frame extended-env)) => (mlist 'x 'y)]
               [(frame-values (first-frame extended-env)) => (mlist 1 2)]
               [(equal? (second extended-env) (first base-env)) => #t]))

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
               [(lookup-variable-value 'z base-env) =!> #rx"Unbound variable"])))))