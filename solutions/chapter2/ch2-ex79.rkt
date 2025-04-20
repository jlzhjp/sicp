#lang racket/base

(require
  akari-sicp/lib/numeric-tower)

(define (install-equ?-package)
  (put 'equ? '(complex complex)
       (let ([~= (lambda (x y) (< (abs (- x y)) 1e-6))])
         (lambda (x y)
           (and (~= (real-part x) (real-part y))
                (~= (imag-part x) (imag-part y)))))))

(put 'equ? '(rational rational)
     (lambda (x y)
       ;; can not use the generic version here
       ;; as the type tag has been erased
       (define (numer x) (car x))
       (define (denom x) (cdr x))
       (and (= (numer x) (numer y))
            (= (denom x) (denom y)))))
  
(put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (= x y)))

(define (equ? x y) (apply-generic 'equ? x y))

(install-generic-arithmetic-package)
(install-equ?-package)

(module+ test
  (require akari-sicp/lib/testing)
  
  (run-tests
   (describe "exercise 2.79"
     (it "scheme number - scheme number"
       (expect
        [(equ? 1 1) => #t]
        [(equ? 1 2) => #f]))
     (it "rational - rational"
       (expect
        [(equ? (make-rat 1 2) (make-rat 1 2)) => #t]
        [(equ? (make-rat 1 2) (make-rat 1 3)) => #f]))
     (it "complex - complex"
       (expect
        [(equ? (make-complex-from-real-imag 3 4)
               (make-complex-from-mag-ang 5 (atan 4 3)))
         => #t]
        [(equ? (make-complex-from-real-imag 4 5)
               (make-complex-from-real-imag 3 4))
         => #f])))))