#lang racket/base

(require racket/match
         (prefix-in ex03: "ch4-ex03.rkt")
         (only-in "ch4-ex08.rkt" eval-let))

(define (scan-out-defines body)
  (let loop ([exps body] [vars '()] [new-body '()])
    (match exps
      [(cons (list 'define (list var args ...) lambda-body ...) _)
       (loop (cdr exps)
             (cons var vars)
             (cons `(set! ,var (lambda ,args ,@lambda-body)) new-body))]
      [(cons (list 'define var val) _)
       (loop (cdr exps)
             (cons var vars)
             (cons `(set! ,var ,val) new-body))]
      [(cons exp _)
       (loop (cdr exps)
             vars
             (cons exp new-body))]
      ['() (values (reverse vars) (reverse new-body))])))

(module+ test
  (require akari-sicp/lib/testing
           akari-sicp/lib/mcons)

  (define declaration
    '(define (f x)
       (define (even? n)
         (if (= n 0)
             #t
             (odd? (- n 1))))
       (define (odd? n)
         (if (= n 0)
             #f
             (even? (- n 1))))

       (define x 3)
       (define y 4)

       (cons (even? x) (odd? y))))

  (define scan-out-defines-tests
    (describe "test `scan-out-defines`"
      (it "shoud extract bindings and body correctly"
        (match-define (list 'define sig body ...) declaration)
        (define-values (bindings new-body) (scan-out-defines body))
        (expect [bindings => (list 'even? 'odd? 'x 'y)]
                [new-body => (list '(set! even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                                   '(set! odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                                   '(set! x 3)
                                   '(set! y 4)
                                   '(cons (even? x) (odd? y)))])))))

(define (transform-body body)
  (define-values (vars new-body) (scan-out-defines body))
  (cond [(null? vars) body] ;; important: if no internal definitions, return the original body
        [else
         (define hoisted-definitions (map (lambda (var) `(,var '*unassigned*)) vars))
         `((let ,hoisted-definitions ,@new-body))]))

(module+ test
  (define
    transform-body-tests
    (describe "test `transform-body`"
      (it "should hoist variables correctly"
        (match-define (list 'define sig body ...) declaration)
        (expect [(transform-body body)
                 => '((let ([even? '*unassigned*]
                            [odd? '*unassigned*]
                            [x '*unassigned*]
                            [y '*unassigned*])
                        (set! even? (lambda (n)
                                      (if (= n 0)
                                          #t
                                          (odd? (- n 1)))))
                        (set! odd? (lambda (n)
                                     (if (= n 0)
                                         #f
                                         (even? (- n 1)))))
                        (set! x 3)
                        (set! y 4)
                        (cons (even? x) (odd? y))))]))
      (it "should return original body if no internal definition"
        (define body '((+ x y) (+ y z)))
        (expect [(transform-body body) => body])))))

(define (eval datum)
  (parameterize
      ([ex03:compound-procedure-handler
        (let ([super (ex03:compound-procedure-handler)])
          (match-lambda
            ['make-procedure
             (lambda (parameters body env)
               ((super 'make-procedure) parameters (transform-body body) env))]
            [otherwise (super otherwise)]))]
       [ex03:environment-handler
        (let ([super (ex03:environment-handler)])
          (match-lambda
            ['lookup-variable-value
             (lambda (var env)
               (let ([val ((super 'lookup-variable-value) var env)])
                 (if (eq? val '*unassigned*)
                     (error 'lookup-variable-value "variable used before bound to its value: ~a" var)
                     val)))]
            [otherwise (super otherwise)]))]
       [ex03:special-form-handlers
        (hash-set (ex03:special-form-handlers)
                  'let eval-let)])
    (ex03:eval datum)))


(module+ test
  (define eval-inner-definition-tests
    (describe "evaluate inner definition"
      (it "should evaluate to correct result"
        (eval declaration)
        (expect [(eval '(f 0)) => (mcons #f #f)]))
      (it "should throw error if variable used before declaration"
        (eval '(define (g x)
                 (define y z)
                 (define z x)
                 (cons y z)))
        (expect [(eval '(g 0)) =!> #rx"variable used before bound to its value"])))))

(module+ test
  (run-tests
   (describe "exercise 4.16"
     scan-out-defines-tests
     transform-body-tests
     eval-inner-definition-tests)))