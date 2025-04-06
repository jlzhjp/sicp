#lang racket/base

(provide eval
         apply
         make-primitive
         make-procedure
         current-environment
         special-form-handlers
         extend-environment
         set-variable-value!
         lookup-variable-value
         setup-base-environment)

(require racket/match
         (only-in akari-sicp/lib/common apply-in-underlying-scheme))

(require (prefix-in env: "ch4-ex11.rkt"))

(define (make-primitive proc) (list 'primitive proc))
(define (primitive-procedure? proc) (match proc [(list 'primitive _) #t] [_ #f]))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))


(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? proc) (match proc [(list 'procedure _ _ _) #t] [_ #f]))
(define (procedure-parameters proc) (cadr proc))
(define (procedure-body proc) (caddr proc))
(define (procedure-environment proc) (cadddr proc))


(define (set-variable-value! var val)
  (env:set-variable-value! var val (current-environment)))

(define (lookup-variable-value var)
  (env:lookup-variable-value var (current-environment)))

(define (define-variable! var val)
  (env:define-variable! var val (current-environment)))

(define (extend-environment vars vals env)
  (env:extend-environment vars vals env))

(define the-empty-environment env:the-empty-environment)

(define primitive-procedures
  (list (list 'car (make-primitive mcar))
        (list 'cdr (make-primitive mcdr))
        (list 'cons (make-primitive mcons))
        (list 'null? (make-primitive null?))
        (list '+ (make-primitive +))
        (list '- (make-primitive -))
        (list '* (make-primitive *))
        (list '/ (make-primitive /))
        (list '= (make-primitive =))
        (list '> (make-primitive >))
        (list '< (make-primitive <))
        (list '>= (make-primitive >=))
        (list '<= (make-primitive <=))
        (list 'eq? (make-primitive eq?))
        (list 'eqv? (make-primitive eqv?))))

(define (setup-base-environment)
  (extend-environment
   (map car primitive-procedures)
   (map cadr primitive-procedures)
   the-empty-environment))

(define (setup-base-special-form-handlers)
  (hasheq
   'quote text-of-quotation
   'lambda eval-lambda
   'if eval-if
   'begin eval-begin
   'define eval-definition
   'set! eval-assignment))


(define (true? x) (not (false? x)))
(define (false? x) (eq? x #f))


(define ((text-of-quotation _eval) exp)
  (define (cons->mcons lst)
    (let loop ([elements lst])
      (cond [(not (pair? elements)) elements]
            [(null? elements) '()]
            [(pair? (car elements))
             (mcons (cons->mcons (car elements)) (loop (cdr elements)))]
            [else
             (mcons (car elements) (loop (cdr elements)))])))
  
  (match exp
    [(list 'quote datum) (cons->mcons datum)]
    [_ (error 'text-of-quotation "expected a quote expression, got ~a" exp)]))

(define ((eval-if eval) exp)
  (match exp
    [(list 'if predicate consequent alternative)
     (if (true? (eval predicate))
         (eval consequent)
         (eval alternative))]
    [_ (error 'eval-if "expected an if expression, got ~a" exp)]))

(define ((eval-sequence eval) exps)
  (define last-exp? (null? (cdr exps)))
  (cond [last-exp? (eval (car exps))]
        [else (eval (car exps))
              ((eval-sequence eval) (cdr exps))]))

(define ((eval-begin eval) exp)
  (match exp
    [(list 'begin exps ...)
     ((eval-sequence eval) exps)]
    [_ (error 'eval-begin "expected a begin expression, got ~a" exp)]))

(define ((eval-assignment eval) exp)
  (match exp
    [(list 'set! variable value)
     (set-variable-value! variable (eval value)) ]
    [_ (error 'eval-assignment "expected a set! expression, got ~a" exp)]))

(define ((eval-definition eval) exp)
  (match exp
    [(list 'define (list fun parameters ...) body ...)
     (define-variable! fun (eval `(lambda ,parameters ,@body)))]
    [(list 'define variable value)
     (define-variable! variable (eval value))]
    [_ (error 'eval-definition "expected a define expression, got ~a" exp)]))

(define ((eval-lambda _eval) exp)
  (match exp
    [(list 'lambda parameters body ...)
     (make-procedure parameters body (current-environment))]
    [_ (error 'eval-lambda " expected a lambda expression, got ~a" exp)]))

(define (self-evaluating? exp) (or (number? exp) (string? exp) (boolean? exp)))

(define (variable? exp) (symbol? exp))

(define (application? exp) (pair? exp))

(define (list-of-values exps) (map eval exps))

(define (eval exp)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp)]
        [(hash-has-key? (special-form-handlers) (car exp))
         (((hash-ref (special-form-handlers) (car exp)) eval) exp)]
        [(application? exp)
         (apply (eval (car exp)) (list-of-values (cdr exp)))]
        [else (error 'eval "Unknown expression type" exp)]))

(define (with-environment env thunk)
  (parameterize ([current-environment env])
    (thunk)))

(define (apply procedure arguments)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments)]
        [(compound-procedure? procedure)
         (with-environment (extend-environment
                            (procedure-parameters procedure)
                            arguments
                            (procedure-environment procedure))
           (lambda () ((eval-sequence eval) (procedure-body procedure))))]
        [else (error 'apply "Unknown procedure type ~a" procedure)]))

(define current-environment (make-parameter (setup-base-environment)))
(define special-form-handlers (make-parameter (setup-base-special-form-handlers)))

(module+ test
  (require akari-sicp/lib/testing
           compatibility/mlist)

  (define (setup-test-environment)
    (extend-environment
     (map car primitive-procedures)
     (map cadr primitive-procedures)
     the-empty-environment))

  (run-tests
   (describe "Exercise 4.3: Basic Evaluator"
     (it "evaluates self-evaluating expressions"
       (expect [(eval 42) => 42]
               [(eval "hello") => "hello"]))

     (it "handles variable lookup"
       (parameterize ([current-environment
                       (extend-environment
                        '(x y z)
                        '(1 2 3)
                        the-empty-environment)])
         (expect [(eval 'x) => 1]
                 [(eval 'y) => 2]
                 [(eval 'z) => 3])))

     (it "evaluates quote expressions"
       (expect [(eval '(quote hello)) => 'hello]
               [(eval '(quote (1 2 3))) => (mlist 1 2 3)]))

     (it "applies primitive procedures"
       (parameterize ([current-environment (setup-test-environment)])
         (expect [(eval '(car (quote (1 2 3)))) => 1]
                 [(eval '(cdr (quote (1 2 3)))) => (mlist 2 3)]
                 [(eval '(cons 1 (quote (2 3)))) => (mlist 1 2 3)]
                 [(eval '(null? (quote ()))) => #t]
                 [(eval '(null? (quote (1)))) => #f]
                 [(eval '(+ 2 3)) => 5]
                 [(eval '(- 5 2)) => 3]
                 [(eval '(* 2 3)) => 6]
                 [(eval '(/ 6 2)) => 3]
                 [(eval '(= 2 2)) => #t])))

     (it "evaluates if expressions"
       (parameterize ([current-environment (setup-test-environment)])
         (expect [(eval '(if #t 1 2)) => 1]
                 [(eval '(if #f 1 2)) => 2]
                 [(eval '(if (= 2 2) 'yes 'no)) => 'yes]
                 [(eval '(if (= 2 3) 'yes 'no)) => 'no])))

     (it "evaluates begin expressions"
       (parameterize ([current-environment (setup-test-environment)])
         (eval '(define x 0))
         (eval '(begin (set! x 1) (set! x (+ x 1))))
         (expect [(eval 'x) => 2])))

     (it "evaluates set! expressions"
       (parameterize ([current-environment (setup-test-environment)])
         (eval '(define x 1))
         (eval '(set! x 42))
         (expect [(eval 'x) => 42])))

     (it "evaluates define expressions"
       (parameterize ([current-environment (setup-test-environment)])
         (eval '(define x 10))
         (expect [(eval 'x) => 10])

         (eval '(define (double y) (+ y y)))
         (expect [(eval '(double 5)) => 10])))

     (it "evaluates lambda expressions and applies compound procedures"
       (parameterize ([current-environment (setup-test-environment)])
         (eval '(define square (lambda (x) (* x x))))
         (expect [(eval '(square 5)) => 25])

         (eval '(define (factorial n)
                  (if (= n 0)
                      1
                      (* n (factorial (- n 1))))))
         (expect [(eval '(factorial 5)) => 120]))))))
