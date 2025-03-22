#lang racket/base

(provide stream-prefix-=
         stream-exact-=
         check-stream-prefix-=
         check-stream-exact-=
         check-output
         check-normalized-output
         it.output
         expect
         run-tests
         (all-from-out rackunit)
         (rename-out [test-suite describe]
                     [test-case it]
                     [test-check it.check]
                     [test-pred it.pred]
                     [test-equal? it.equal?]
                     [test-eq? it.eq?]
                     [test-eqv? it.eqv?]
                     [test-= it.=]
                     [test-true it.true]
                     [test-false it.false]
                     [test-not-false it.not-false]
                     [test-exn it.exn]
                     [test-not-exn it.not-exn]))

(require "stream.rkt"
         racket/string
         racket/contract
         racket/port
         rackunit
         syntax/parse/define
         (only-in rackunit/text-ui run-tests)
         (for-syntax racket/base))

(define-syntax (expect-single-rule stx)
  (syntax-parse stx
    [(_ (actual:expr (~literal =>) expected:expr))
     (syntax/loc stx (check-equal? actual expected))]
    [(_ (actual:expr (~literal ~>) expected:expr))
     (syntax/loc stx (check-= actual expected 1e-6))]
    [(_ (thunk:expr (~literal =$>) expected:expr))
     (syntax/loc stx (check-normalized-output thunk expected))]))

(define-syntax (expect stx)
  (syntax-parse stx
    [(_ rule:expr ... )
     (with-syntax ([(rules ...)
                    (for/list ([r (syntax->list #'(rule ...))])
                      (quasisyntax/loc stx (expect-single-rule #,r)))])
       #'(begin rules ...))]))

(define/contract (it.output name thunk expected-lines)
  (-> string? procedure? (listof string?) void?)
  (test-case name
             (check-normalized-output thunk expected-lines)))

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
         (stream-exact-= (stream-cdr stream) (cdr lst))]
        [else #f]))

(define (check-stream-exact comp stream lst)
  (cond
    [(and (null? lst) (stream-null? stream)) #t]  ;; Both empty - success
    [(null? lst)
     (fail-check (format "stream longer than expected list:\n  stream prefix: ~v\n  list: ~v"
                         (collect-stream stream 10) lst))]
    [(stream-null? stream)
     (fail-check (format "stream shorter than expected list:\n  stream: ~v\n  list: ~v"
                         '() lst))]
    [(not (comp (stream-car stream) (car lst)))
     (fail-check (format "stream element mismatch at position:\n  stream element: ~v\n  list element: ~v"
                         (stream-car stream) (car lst)))]
    [else (check-stream-exact comp (stream-cdr stream) (cdr lst))]))

(define (check-stream-prefix comp stream lst)
  (cond
    [(null? lst) #t]  ;; List is exhausted - success for prefix check
    [(stream-null? stream)
     (fail-check (format "stream shorter than expected prefix:\n  stream: ~v\n  remaining list: ~v"
                         '() lst))]
    [(not (comp (stream-car stream) (car lst)))
     (fail-check (format "stream prefix mismatch at position:\n  stream element: ~v\n  list element: ~v"
                         (stream-car stream) (car lst)))]
    [else (check-stream-prefix comp (stream-cdr stream) (cdr lst))]))

(define-check (check-stream-prefix-= stream lst)
  (check-stream-prefix = stream lst))

(define-check (check-stream-exact-= stream lst)
  (check-stream-exact = stream lst))

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

