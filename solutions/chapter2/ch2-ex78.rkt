#lang racket/base

(provide attach-tag
         type-tag
         contents)

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond
    [(exact-integer? datum) 'scheme-number]
    [(pair? datum) (car datum)]
    [else (error 'type-tag "bad tagged datum ~a" datum)]))

(define (contents datum)
  (cond
    [(exact-integer? datum) datum]
    [(pair? datum) (cdr datum)]
    [else (error 'contents "bad tagged datum ~a" datum)]))