# 第 2 章 构造数据抽象

## 实例 有理数的算术运算

```scheme
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
```

## 练习 2.1 改进 `make-rat` 的实现

```scheme
(define (make-rat n d)
  (let [(divisor (gcd n d))]
    (let [(n* (/ n divisor))
          (d* (/ d divisor))]
      (cond [(> d* 0) (cons n* d*)]
            [(< d* 0) (cons (- n*) (- d*))]))))
```

# 练习 2.2 线段中点
```scheme
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
(print-point mid)
```

## 练习 2.3 平面矩形的表示
```scheme
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

(display "Width: ") (display (rect-width rect)) (newline)
(display "Height: ") (display (rect-height rect)) (newline)
(display "Area: ") (display (rect-area rect)) (newline)
(display "Perimeter: ") (display (rect-perimeter rect)) (newline)
```

## 实例 序对的一种过程性表示
```scheme
(define (cons x y)
  (define (dispatch m)
    (cond [(= m 0) x]
          [(= m 1) y]
          [else (error "Argument not 0 or 1 -- CONS" m)]))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

(define pair (cons 1 2))
(display (car pair))
(display (cdr pair))
```

## 练习 2.4 序对的另一种过程性表示
```scheme
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(car z)
(car (lambda (m) (m x y)))
(((lambda (m) (m x y)) (lambda (p q) p)))
((lambda (p q) p) x y)
p
```

## 练习 2.5 将 $a$ 和 $b$ 的序对表示为乘积 $2^a \cdot 3^b$ 对应的整数

```scheme
(define (cons-int a b) (* (expt 2 a) (expt 3 b)))

(define (car-int z)
  (if (= 0 (remainder z 3))
      (car-int (/ z 3))
      (log z 2)))

(define (cdr-int z)
  (log (/ z (expt 2 (car-int z))) 3))

(define pair (cons-int 6 3))
(display (car-int pair)) (display " ") (display (cdr-int pair))
```

## 练习 2.6 丘奇数

```scheme
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

(display (church->number (add one two)))
```

## 练习 2.7 定义 `upper-bound` 和 `lower-bound`
```scheme
(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))
```

## 练习 2.8 定义 `sub-interval`
```scheme
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
```

## 练习 2.9 区间的宽度
```scheme
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
```

$$
\def\width{\mathrm{width}}
\def\sub{\mathrm{sub}}
\def\add{\mathrm{add}}


\begin{aligned}
& (\width \ (\add\ [x_1,y_1]\ [x_2,y_2])) \\
\implies & (\width\ [x_1 + x_2, y_1 + y_2]) \\
\implies & \frac{(y_1 + y_2) - (x_1 + x_2)}{2} \\
\implies & \frac{y_1 - x_1}{2} + \frac{y_2 - x_2}{2} \\
\implies & (\width\ [x_1, y_1]) + (\width\ [x_2, y_2])
\end{aligned} \\

\\[10pt]

\begin{aligned}
& (\width \ (\sub\ [x_1,y_1]\ [x_2,y_2])) \\
\implies & (\width\ [x_1 - y_2, y_1 - x_2]) \\
\implies & \frac{(y_1 - x_2) - (x_1 - y_2)}{2} \\
\implies & \frac{y_1 - x_1}{2} + \frac{y_2 - x_2}{2} \\
\implies & (\width\ [x_1, y_1]) + (\width\ [x_2, y_2])
\end{aligned}
$$

## 练习 2.10 处理被除区间跨过 0 的情况
```scheme
(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "can not divide a span that spans zero.")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))
```

## 练习 2.11 将 `mul-interval` 分解为 9 种情况
TODO