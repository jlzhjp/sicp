#lang racket/base

(provide cond->if
         let->combination)

(define (make-begin seq) (cons 'begin seq))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (sequence->exp seq)
  (define (last-exp? seq) (null? (cdr seq)))
  (define (first-exp seq) (car seq))
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

; (cond [<condition_1> <action_1>]
;       [<condition_2> <action_2>]
;       [else <action_else>])
(define (cond->if exp)
  (define (cond-clauses exp) (cdr exp))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause) (cdr clause))
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (let ([first (car clauses)]
              [rest (cdr clauses)])
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last: COND->IF"
                         clauses))
              (make-if (cond-predicate first)
                       (sequence->exp (cond-actions first))
                       (expand-clauses rest))))))
  (expand-clauses (cond-clauses exp)))

(define (let->combination let-statement)
  (define (let-assignments let-statement) (cadr let-statement))
  (define (let-body let-statement) (caddr let-statement))
  (define (assignment-var assignment) (car assignment))
  (define (assignment-exp assignment) (cadr assignment))

  (let* ([assignments (let-assignments let-statement)]
         [body (let-body let-statement)]
         [assignment-vars (map assignment-var assignments)]
         [assignment-exps (map assignment-exp assignments)])
    (cons (list 'lambda assignment-vars body) assignment-exps)))
