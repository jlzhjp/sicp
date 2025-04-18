#lang racket/base

(provide primitive-procedure@
         compound-procedure@
         evaluator-environment@
         metacircular-evaluator@
         evaluator-compound@)

(require racket/match
         racket/unit
         (only-in akari-sicp/lib/common apply-in-underlying-scheme)
         "signatures.rkt"
         "ch4-ex12.rkt")

(define-unit primitive-procedure@
  (import)
  (export primitive-procedure^)

  (define (make-primitive proc) (list 'primitive proc))
  (define (primitive-procedure? proc) (match proc [(list 'primitive _) #t] [_ #f]))
  (define (primitive-implementation proc) (cadr proc))
  (define (apply-primitive-procedure proc args)
    (apply-in-underlying-scheme (primitive-implementation proc) args)))

(define-unit compound-procedure@
  (import)
  (export compound-procedure^)

  (define (make-procedure parameters body env) (list 'procedure parameters body env))
  (define (compound-procedure? proc) (match proc [(list 'procedure _ _ _) #t] [_ #f]))
  (define procedure-parameters cadr)
  (define procedure-body caddr)
  (define procedure-environment cadddr))

(define-unit metacircular-evaluator@
  (import primitive-procedure^ compound-procedure^ evaluator-environment^)
  (export metacircular-evaluator^)

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

  (define (true? x) (not (false? x)))
  (define (false? x) (eq? x #f))

  (define (cons->mcons lst)
    (let loop ([elements lst])
      (cond [(not (pair? elements)) elements]
            [(null? elements) '()]
            [(pair? (car elements))
             (mcons (cons->mcons (car elements)) (loop (cdr elements)))]
            [else
             (mcons (car elements) (loop (cdr elements)))])))

  (define text-of-quotation
    (match-lambda
      [(list 'quote datum) (cons->mcons datum)]
      [otherwise (error 'text-of-quotation "expected a quote expression, got ~a" otherwise)]))

  (define eval-if
    (match-lambda
      [(list 'if predicate consequent alternative)
       (if (true? (eval predicate))
           (eval consequent)
           (eval alternative))]
      [otherwise (error 'eval-if "expected an if expression, got ~a" otherwise)]))

  (define (eval-sequence seq)
    (define last-exp? (null? (cdr seq)))
    (cond [last-exp? (eval (car seq))]
          [else (eval (car seq))
                (eval-sequence  (cdr seq))]))

  (define eval-begin
    (match-lambda
      [(list 'begin statements ...) (eval-sequence statements)]
      [otherwise (error 'eval-begin "expected a begin expression, got ~a" otherwise)]))

  (define eval-assignment
    (match-lambda
      [(list 'set! var val) (set-variable-value! var (eval val) (current-environment)) ]
      [otherwise (error 'eval-assignment "expected a set! expression, got ~a" otherwise)]))

  (define eval-definition
    (match-lambda
      [(list 'define (list fun params ...) body ...)
       (define-variable! fun (eval `(lambda ,params ,@body)) (current-environment))]
      [(list 'define var val)
       (define-variable! var (eval val) (current-environment))]
      [otherwise (error 'eval-definition "expected a define expression, got ~a" otherwise)]))

  (define eval-lambda
    (match-lambda
      [(list 'lambda params body ...)
       (make-procedure params body (current-environment))]
      [otherwise (error 'eval-lambda " expected a lambda expression, got ~a" otherwise)]))

  (define (self-evaluating? exp) (or (number? exp) (string? exp) (boolean? exp)))

  (define (variable? exp) (symbol? exp))

  (define (application? exp) (pair? exp))

  (define (list-of-values exps) (map eval exps))

  (define (setup-base-special-form-handlers)
    (hasheq
     'quote text-of-quotation
     'lambda eval-lambda
     'if eval-if
     'begin eval-begin
     'define eval-definition
     'set! eval-assignment))

  (define current-environment (make-parameter (setup-base-environment)))
  (define special-form-handlers (make-parameter (setup-base-special-form-handlers)))

  (define (eval exp)
    (cond [(self-evaluating? exp) exp]
          [(variable? exp) (lookup-variable-value exp (current-environment))]
          [(hash-has-key? (special-form-handlers) (car exp))
           ((hash-ref (special-form-handlers) (car exp)) exp)]
          [(application? exp)
           (apply (eval (car exp)) (list-of-values (cdr exp)))]
          [else (error 'eval "Unknown expression type" exp)]))

  (define (application-environment proc args)
    (extend-environment (procedure-parameters proc) args (procedure-environment proc)))

  (define (apply procedure arguments)
    (cond [(primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments)]
          [(compound-procedure? procedure)
           (parameterize ([current-environment (application-environment procedure arguments)])
             (eval-sequence  (procedure-body procedure)))]
          [else (error 'apply "Unknown procedure type ~a" procedure)])))

(define-compound-unit/infer evaluator-compound@
  (import)
  (export primitive-procedure^
          compound-procedure^
          evaluator-environment^
          metacircular-evaluator^)
  (link primitive-procedure@
        compound-procedure@
        evaluator-environment@
        metacircular-evaluator@))

(module+ test
  (require akari-sicp/lib/testing
           akari-sicp/lib/mcons)

  (define-values/invoke-unit/infer evaluator-compound@)

  (run-tests
   (describe "exercise 4.3"
     (it "evaluates self-evaluating expressions"
       (expect [(eval 42) => 42]
               [(eval "hello") => "hello"]))

     (it "handles variable lookup"
       (parameterize ([current-environment
                       (extend-environment
                        '(x y z)
                        '(1 2 3)
                        (current-environment))])
         (expect [(eval 'x) => 1]
                 [(eval 'y) => 2]
                 [(eval 'z) => 3])))

     (it "evaluates quote expressions"
       (expect [(eval '(quote hello)) => 'hello]
               [(eval '(quote (1 2 3))) => (mlist 1 2 3)]))

     (it "applies primitive procedures"
       (expect [(eval '(car (quote (1 2 3)))) => 1]
               [(eval '(cdr (quote (1 2 3)))) => (mlist 2 3)]
               [(eval '(cons 1 (quote (2 3)))) => (mlist 1 2 3)]
               [(eval '(null? (quote ()))) => #t]
               [(eval '(null? (quote (1)))) => #f]
               [(eval '(+ 2 3)) => 5]
               [(eval '(- 5 2)) => 3]
               [(eval '(* 2 3)) => 6]
               [(eval '(/ 6 2)) => 3]
               [(eval '(= 2 2)) => #t]))

     (it "evaluates if expressions"
       (expect [(eval '(if #t 1 2)) => 1]
               [(eval '(if #f 1 2)) => 2]
               [(eval '(if (= 2 2) 'yes 'no)) => 'yes]
               [(eval '(if (= 2 3) 'yes 'no)) => 'no]))

     (it "evaluates begin expressions"
       (eval '(define x 0))
       (eval '(begin (set! x 1) (set! x (+ x 1))))
       (expect [(eval 'x) => 2]))

     (it "evaluates set! expressions"
       (eval '(define x 1))
       (eval '(set! x 42))
       (expect [(eval 'x) => 42]))

     (it "evaluates define expressions"
       (eval '(define x 10))
       (expect [(eval 'x) => 10])

       (eval '(define (double y) (+ y y)))
       (expect [(eval '(double 5)) => 10]))

     (it "evaluates lambda expressions and applies compound procedures"
       (eval '(define square (lambda (x) (* x x))))
       (expect [(eval '(square 5)) => 25])

       (eval '(define (factorial n)
                (if (= n 0)
                    1
                    (* n (factorial (- n 1))))))
       (expect [(eval '(factorial 5)) => 120])))))
