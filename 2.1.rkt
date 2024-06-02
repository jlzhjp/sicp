#lang sicp

; 2.1
(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

(define (make-rat n d)
  (let [(divisor (gcd n d))]
    (let [(n* (/ n divisor))
          (d* (/ d divisor))]
      (cond [(> d* 0) (cons n* d*)]
            [(< d* 0) (cons (- n*) (- d*))]))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat one-third)

(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))
(print-rat (make-rat 6 -9))

(gcd 30 9)
