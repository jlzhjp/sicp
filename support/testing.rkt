#lang racket/base

(provide stream-prefix-=
         stream-exact-=
         check-stream-prefix-=
         check-stream-exact-=
         check-output
         check-normalized-output
         (all-from-out rackunit))

(require "stream.rkt"
         racket/string
         racket/port
         rackunit)

(require (for-syntax racket/base))

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

(define (normalize-string str)
  (string-join
   (filter
    (lambda (line) (not (string=? line " ")))
    (map (lambda (line) (string-trim line)) (string-split str "\n")))
   "\n"))

(define (normalized-string=? s1 s2)
  (string=? (normalize-string s1) (normalize-string s2)))

(define-check (check-normalized-output proc expected-lines)
  (let ([output (with-output-to-string proc)]
        [expected (string-join expected-lines "\n")])
    (unless (normalized-string=? output expected)
      (fail-check (format "normalized output mismatch:\n  actual: ~v\nexpected: ~v"
                          (normalize-string output)
                          (normalize-string expected))))))

(define-check (check-output proc expected)
  (let ([output (with-output-to-string proc)])
    (unless (string=? output expected)
      (fail-check (format "output mismatch:\n  actual: ~v\nexpected: ~v"
                          output
                          expected)))))
