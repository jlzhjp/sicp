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
; (print-rat one-half)
(define one-third (make-rat 1 3))
; (print-rat one-third)

; (print-rat (add-rat one-half one-third))
; (print-rat (mul-rat one-half one-third))
; (print-rat (add-rat one-third one-third))
; (print-rat (make-rat 6 -9))

; (gcd 30 9)

(define (make-segment start end) (cons start end))

(define (start-segment seg) (car seg))

(define (end-segment seg) (cdr seg))

(define (make-point x-point y-point) (cons x-point y-point))

(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define (midpoint-segment seg)
  (make-point
   (/ (+ (x-point (start-segment seg))
         (x-point (end-segment seg)))
      2)
   (/ (+ (y-point (start-segment seg))
         (y-point (end-segment seg)))
      2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define line (make-segment (make-point 1 2) (make-point 3 4)))
(define mid (midpoint-segment line))
; (print-point mid)

(define (make-rect point-lt point-rb) (cons point-lt point-rb))
(define (rect-point-lt rect) (car rect))
(define (rect-point-rb rect) (cdr rect))

(define (rect-width rect) (-(x-point (rect-point-rb rect))
                            (x-point (rect-point-lt rect))))

(define (rect-height rect) (- (y-point (rect-point-lt rect))
                              (y-point (rect-point-rb rect))))

(define (rect-area rect) (* (rect-width rect) (rect-height rect)))
(define (rect-perimeter rect) (* 2 (+ (rect-width rect) (rect-height rect))))

(define rect (make-rect (make-point 1 4) (make-point 3 1)))

; (display "Width: ") (display (rect-width rect)) (newline)
; (display "Height: ") (display (rect-height rect)) (newline)
; (display "Area: ") (display (rect-area rect)) (newline)
; (display "Perimeter: ") (display (rect-perimeter rect)) (newline)

(define (cons-int a b) (* (expt 2 a) (expt 3 b)))

(define (car-int z)
  (if (= 0 (remainder z 3))
      (car-int (/ z 3))
      (log z 2)))

(define (cdr-int z)
  (log (/ z (expt 2 (car-int z))) 3))

(define pair (cons-int 6 3))
; (display (car-int pair)) (display " ") (display (cdr-int pair))

(define zero (lambda (f)
               (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one (lambda (f)
              (lambda (x)
                (f x))))

(define two (lambda (f)
              (lambda (x)
                (f (f x)))))

(define (add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

