#lang racket/base

(provide stream-prefix-=
         stream-exact-=
         check-stream-prefix-=
         check-stream-exact-=
         check-output
         check-output-trimmed
         lines
         (all-from-out rackunit))

(require "stream.rkt"
         racket/string
         racket/port
         rackunit)

(define (stream-prefix-= stream lst)
  (cond [(null? lst) #t]
        [(stream-null? stream) #f]
        [(= (stream-car stream) (car lst))
         (stream-prefix-= (stream-cdr stream) (cdr lst))]
        [else #f]))

(define (stream-exact-= stream lst)
  (cond [(and (null? lst) (stream-null? stream)) #t]
        [(or (null? lst) (stream-null? stream)) #f]
        [(= (stream-car stream) (car lst))
         (stream-prefix-= (stream-cdr stream) (cdr lst))]
        [else #f]))

(define-check (check-stream-prefix-= stream lst)
  (check-true (stream-prefix-= stream lst)))

(define-check (check-stream-exact-= stream lst)
  (check-true (stream-exact-= stream lst)))

(define-syntax-rule (check-output expected proc ...)
  (let ([output (with-output-to-string (lambda () proc ...))])
    (check-equal? output expected)))

(define-syntax-rule (check-output-trimmed expected proc ...)
  (let ([output (trimmed-lines (with-output-to-string (lambda () proc ...)))])
    (check-equal? output expected)))

(define (trimmed-lines text)
  (string-join
   (filter non-empty-string?
           (map (lambda (line) (string-trim line))
                (string-split text "\n")))
   "\n"
   #:after-last "\n"))

(define (lines . strs)
  (string-join strs "\n" #:after-last "\n"))