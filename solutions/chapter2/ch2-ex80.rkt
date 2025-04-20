#lang racket/base

(require
  akari-sicp/lib/numeric-tower)

(define (install-=zero?-package)
  (put '=zero? '(scheme-number)
       (lambda (x) (zero? x)))

  (put '=zero? '(rational)
       (lambda (x)
         (define (numer x) (car x))
         (= (numer x) 0)))
  
  (put '=zero? '(complex)
       (lambda (x)
         (and (= (real-part x) 0)
              (= (imag-part x) 0)))))

(define (=zero? x)
  (apply-generic '=zero? x))

(install-generic-arithmetic-package)
(install-=zero?-package)

(module+ test
  (require akari-sicp/lib/testing)
  
  (run-tests
   (describe "exercise 3.80"
     (it "scheme number =zero?"
       (expect
        [(=zero? 0) => #t]
        [(=zero? 1) => #f]))
     (it "rational =zero?"
       (expect
        [(=zero? (make-rat 0 2)) => #t]
        [(=zero? (make-rat 1 2)) => #f]))
     (it "complex =zero?"
       (expect
        [(=zero? (make-complex-from-real-imag 1 2)) => #f]
        [(=zero? (make-complex-from-mag-ang 1 1)) => #f]
        [(=zero? (make-complex-from-real-imag 0 0)) => #t]
        [(=zero? (make-complex-from-mag-ang 0 1)) => #t])))))