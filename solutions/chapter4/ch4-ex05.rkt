#lang racket/base

(require racket/match
         racket/unit
         "ch4-ex03.rkt"
         "signatures.rkt")

(define-unit cond-extension@
  (import metacircular-evaluator^)
  (export cond-extension^)

  (define eval-cond
    (match-lambda
      [(list 'cond cond-clauses ...) (eval (expand-cond-clauses cond-clauses))]
      [otherwise (error 'eval-cond "Invalid cond form: ~a" otherwise)])))

(define (expand-cond-clauses clauses)
  (if (null? clauses)
      #f
      (match (car clauses)
        [(list 'else body ...)
         `(begin ,@body)]
        [(list predicate '=> recipient)
         `((lambda (predicate-result)
             (if predicate-result
                 (,recipient predicate-result)
                 ,(expand-cond-clauses (cdr clauses))))
           ,predicate)]
        [(list predicate body ...)
         `(if ,predicate
              (begin ,@body)
              ,(expand-cond-clauses (cdr clauses)))]
        [_ (error 'eval-cond "Invalid cond clause: ~a" (car clauses))])))

(module+ test
  (require akari-sicp/lib/testing)

  (define-values/invoke-unit/infer
    (link cond-extension@ evaluator-compound@))

  (special-form-handlers (hash-set (special-form-handlers) 'cond eval-cond))

  (run-tests
   (describe "expand-cond-clauses"
     (it "transforms basic cond to nested if expressions"
       (let ([datum '(cond (#t 1) (#f 2))])
         (expect [(expand-cond-clauses (cdr datum))
                  => '(if #t (begin 1) (if #f (begin 2) #f))])))

     (it "transforms arrow syntax cond correctly"
       (let ([datum '(cond (#t => display))])
         (expect [(expand-cond-clauses (cdr datum))
                  => '((lambda (predicate-result)
                         (if predicate-result
                             (display predicate-result)
                             #f))
                       #t)])))
     (describe "cond as a special form"
       (it "evaluates basic cond expressions"
         (parameterize ([current-environment
                         (extend-environment '(a b) '(10 20) (current-environment))])
           (expect [(eval '(cond ((> a 5) a)
                                 ((> b 5) b)
                                 (else 0))) => 10]
                   [(eval '(cond ((> a 15) a)
                                 ((> b 15) b)
                                 (else 0))) => 20])))

       (it "evaluates cond with arrow syntax"
         (parameterize ([current-environment
                         (extend-environment
                          '(a square identity)
                          (list 4
                                (eval '(lambda (x) (* x x)))
                                (eval '(lambda (x) x)))
                          (current-environment))])
           (expect [(eval '(cond (a => square)
                                 (else 0)))
                    => 16]
                   [(eval '(cond ((< a 0) => square)
                                 (else 0)))
                    => 0])))

       (it "handles complex cond expressions with arrow syntax"
         (parameterize ([current-environment
                         (extend-environment
                          '(x y inc double bool->num)
                          (list 5 10
                                (eval '(lambda (n) (+ n 1)))
                                (eval '(lambda (n) (* n 2)))
                                (eval '(lambda (b) (if b 1 0))))
                          (current-environment))])
           (expect [(eval '(cond (x => inc)
                                 ((= y 10) y)
                                 (else 0))) => 6]
                   [(eval '(cond ((= x 5) => bool->num)
                                 ((> y 20) y)
                                 (else 0))) => 1]
                   [(eval '(cond ((> x 10) => inc)
                                 (y => double)
                                 (else 0))) => 20])))))))
