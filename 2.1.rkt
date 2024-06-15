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

(define (church->number n)
  ((n (lambda (x) (+ x 1))) 0))

; (display (church->number (add one two)))

(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ([p1 (* (lower-bound x) (lower-bound y))]
        [p2 (* (lower-bound x) (upper-bound y))]
        [p3 (* (upper-bound x) (lower-bound y))]
        [p4 (* (upper-bound x) (upper-bound y))])
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "can not divide a span that spans zero.")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))
  
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; (define x (make-interval 10 20))
; (define y (make-interval -20 30))
; (div-interval x y)

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

; (define x (make-interval 10 20))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(define (length* items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

(define (reverse items)
  (define (iter xs ys)
    (if (null? xs)
        ys
        (iter (cdr xs) (cons (car xs) ys))))
  (iter items nil))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1))

(define (no-more? coin-values) (null? coin-values))

(define (first-denomination coin-values) (car coin-values))

(define (except-first-denomination coin-values) (cdr coin-values))

(define (cc amount coin-values)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (no-more? coin-values)) 0]
        [else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values))]))

(cc 100 us-coins)


(define (same-parity . items)
  (let ([parity (remainder (car items) 2)])
    (define (helper xs)
      (cond [(null? xs) '()]
            [(= (remainder (car xs) 2) parity)
             (cons (car xs) (helper (cdr xs)))]
            [else (helper (cdr xs))]))
    (helper items)))

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list* items factor)
  (map (lambda (x) (* x factor))
       items))

(define (square-list items)
  (define (square x) (* x x))
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list* items)
  (define (square x) (* x x))
  (map square items))

(define (square-list** items)
  (define (square x) (* x x))
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

; (square-list** '(1 2 3 4))

(define (for-each f items)
  (if (null? (cdr items))
      (f (car items))
      (begin (f (car items)) (for-each f (cdr items)))))

(define (count-leaves x)
  (cond [(null? x) 0]
        [(pair? x) (+ (count-leaves (car x)) (count-leaves (cdr x)))]
        [else 1]))

; (define a '(1 3 (5 7) 9))
; (define b '((7)))
; (define c '(1 (2 (3 (4 (5 (6 7)))))))

(define (deep-reverse items)
  (define (iter rest answer)
    (if (null? rest)
        answer
        (let ([cur (if (pair? (car rest))
                       (deep-reverse (car rest))
                       (car rest))])
          (iter (cdr rest) (cons cur answer)))))
  (iter items nil))

(define (fringe x)
  (cond [(null? x) '()]
        [(pair? x) (append (fringe (car x)) (fringe (cdr x)))]
        [else (list x)]))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile) (list-ref mobile 0))
(define (right-branch mobile) (list-ref mobile 1))
(define (branch-length branch) (list-ref branch 0))
(define (branch-structure branch) (list-ref branch 1))
(define (total-weight mobile)
  (if (pair? mobile)
      (+ (total-weight (branch-structure (left-branch mobile)))
         (total-weight (branch-structure (right-branch mobile))))
      mobile))

(define mobile-x
  (make-mobile (make-branch 1 (make-mobile (make-branch 1 1)
                                           (make-branch 1 2)))
               (make-branch 1 (make-mobile (make-branch 1 3)
                                           (make-branch 1 4)))))

(define mobile-balanced
  (make-mobile (make-branch 2 (make-mobile (make-branch 1 2)
                                           (make-branch 1 2)))
               (make-branch 1 (make-mobile (make-branch 1 4)
                                           (make-branch 1 4)))))

(define (is-balance mobile)
  (if (pair? mobile)
      (let ([left-len (branch-length (left-branch mobile))]
            [right-len (branch-length (right-branch mobile))]
            [left-struct (branch-structure (left-branch mobile))]
            [right-struct (branch-structure (right-branch mobile))])
        (and (= (* left-len (total-weight left-struct))
                (* right-len (total-weight right-struct)))
             (is-balance left-struct)
             (is-balance right-struct)))
      #t))


(define (scale-tree tree factor)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (* tree factor)]
        [else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor))]))

(define (scale-tree* tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))




