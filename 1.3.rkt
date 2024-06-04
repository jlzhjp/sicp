#lang sicp

; (define (sum-integers a b)
;   (if (> a b)
;       0
;       (+ a (sum-integers (+ a 1) b))))

(define (cube a) (* a a a))

; (define (sum-cubes a b)
;   (if (> a b)
;       0
;       (+ (cube a))))

; (define (pi-sum a b)
;   (if (> a b)
;       0
;       (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0))  add-dx b)
     dx))

(define (simpson-integral f a b n)
  (define (h) (/ (- b a) n))
  (define (y k)
    (f (+ a (* k (h)))))
  (define (t k)
    (cond [(or (= k 0) (= k n)) (y k)]
          [(even? k) (* 2 (y k))]
          [else (* 4 (y k))]))
  (* (/ (h) 3.0)
     (sum t 0 inc n)))

(define (sum* term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (+ a 1) (+ (term a) result))))
  (iter a 0))

(define (simpson-integral* f a b n)
  (define (h) (/ (- b a) n))
  (define (y k)
    (f (+ a (* k (h)))))
  (define (t k)
    (cond [(or (= k 0) (= k n)) (y k)]
          [(even? k) (* 2 (y k))]
          [else (* 4 (y k))]))
  (* (/ (h) 3.0)
     (sum* t 0 inc n)))

(define (product factor a next b)
  (if (> a b)
      1
      (* (factor a) (product factor (next a) next b))))

(define (product* factor a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (+ a 1) (* (factor a) result))))
  (iter a 1))

(define (wallis-product n)
  (define (inc x) (+ 1 x))
  (define (fact n) (* (/ (* 2 n)
                         (- (* 2 n) 1))
                      (/ (* 2 n)
                         (+ (* 2 n) 1))))
  (product* fact 1.0 inc n))

(define (accmulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accmulate combiner
                           null-value
                           term
                           (next a)
                           next
                           b))))

(define (sum-alt term a next b)
  (accmulate + 0 term a next b))

(define (prod-alt factor a next b)
  (accmulate * 1 factor a next b))

(define (accmulate* combiner null-value term a next b)
  (define (iter n result)
    (if (> n b)
        result
        (iter (next n) (combiner (term n) result))))
  (iter a null-value))

(define (sum-alt* term a next b)
  (accmulate* + 0 term a next b))

(define (prod-alt* factor a next b)
  (accmulate* * 1 factor a next b))

(define (filtered-accumulate filter
                             combiner
                             null-value
                             term
                             a
                             next
                             b)
  (define (iter n result)
    (cond [(> n b) result]
          [(not (filter (term n))) (iter (next n) result)]
          [else (iter (next n) (combiner n result))]))
  (iter a null-value))

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (+ test-divisor 1))]))

(define (prime? n)
  (if (= n 1) #f
      (= n (smallest-divisor n))))

(define (prime-sum a b)
  (filtered-accumulate prime? + 0 identity a inc b))

(define (relative-prime? i n)
  (= (gcd i n) 1))

(define (product-of-relative-prime n)
  (define (relative-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate relative-prime? * 1 identity 1 inc n))


(define (average x y) (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  
  (let ([midpoint (average neg-point pos-point)])
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ([test-value (f midpoint)])
          (cond [(positive? test-value)
                  (search f neg-point midpoint)]
                [(negative? test-value)
                 (search f midpoint pos-point)])))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond [(and (negative? a-value) (positive? b-value))
            (search f a b)]
           [(and (negative? b-value) (positive? a-value))
            (search f b a)]
           [else
            (error "Values are not of opposite sign" a b)])))

; (half-interval-method sin 2.0 4.0)
; (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;                       1.0
;                       2.0)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; (fixed-point cos 1.0)
; (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

; (sqrt 4)


; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

(define (fixed-point* f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (display "; ")
    (let ([next (f guess)])
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; (newline)
; (fixed-point* (lambda (y) (/ (log 1000) (log y))) 1.5)
; (newline)
; (fixed-point* (lambda (y) (average y (/ (log 1000) (log y)))) 1.5)

; 1.37
(define (cont-frac n d k)
  (define (inner i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (inner (+ i 1))))))
  (inner 1))

(define (cont-frac* n d k)
  (define (iter prev i)
    (if (= i 0)
        prev
        (iter (/ (n i) (+ (d i) prev)) (- i 1))))
  (iter 0 k))

; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)
; (cont-frac* (lambda (i) 1.0) (lambda (i) 1.0) 100)


; 1.38

(define (euler n)
  (cont-frac*
   (lambda (i) 1.0)
   (lambda (i)
     (if (= (remainder (+ i 1) 3) 0)
         (* (/ (+ i 1) 3) 2)
         1))
   n))

; (+ (euler 100) 2)

; 1.39
(define (tan-cf x k)
  (cont-frac*
   (lambda (i)
     (if (= i 1) x (- (* x x))))
   (lambda (i)
     (- (* i 2) 1))
   k))

; (tan-cf 3.1415926535 10000)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt* x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-n x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-d* x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt-n* x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

; (newtons-method (cubic 1 1 1) 1)

(define (double f)
  (lambda (x)
    (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f count)
  (define (iter counter inner-result)
    (if (= counter 0)
        inner-result
        (iter (- counter 1) (f inner-result))))
  (lambda (x) (iter count x)))

; ((repeated square 2) 5)

(define (smooth f)
  (define (dx) 0.0001)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))