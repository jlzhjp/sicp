#lang racket/base

; reference: https://mk12.github.io/sicp/exercise/2/3.html#ex2.58

(provide deriv)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eqv? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond [(=number? a1 0) a2]
        [(=number? a2 0) a1]
        [(and (number? a1) (number? a2)) (+ a1 a2)]
        [else (list a1 '+ a2)]))

(define (make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list m1 '* m2)]))

(define (has op exp)
  (and (pair? exp) (memq op exp)))

(define (unwrap exp)
  (if (and (pair? exp) (null? (cdr exp)))
      (car exp)
      exp))

(define (before op exp)
  (define (iter exp)
    (if (eqv? op (car exp))
        '()
        (cons (car exp) (iter (cdr exp)))))
  (unwrap (iter exp)))

(define (after op exp)
  (unwrap (cdr (memq op exp))))

(define (sum? x) (has '+ x))

(define (addend s) (before '+ s))

(define (augend s) (after '+ s))

(define (product? x) (and (not (sum? x)) (has '* x)))

(define (multiplier p) (before '* p))

(define (multiplicand p) (after '* p))

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp)))]
        [else (error "unknown expression type -- DERIV" exp)]))

(module+ test)