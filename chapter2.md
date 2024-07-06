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

## 练习 2.2 线段中点
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
\begin{aligned}
& (\mathrm{width} \ (\mathrm{add}\ [x_1,y_1]\ [x_2,y_2])) \\
\implies & (\mathrm{width}\ [x_1 + x_2, y_1 + y_2]) \\
\implies & \frac{(y_1 + y_2) - (x_1 + x_2)}{2} \\
\implies & \frac{y_1 - x_1}{2} + \frac{y_2 - x_2}{2} \\
\implies & (\mathrm{width}\ [x_1, y_1]) + (\mathrm{width}\ [x_2, y_2])
\end{aligned} \\

\\[10pt]

\begin{aligned}
& (\mathrm{width} \ (\mathrm{sub}\ [x_1,y_1]\ [x_2,y_2])) \\
\implies & (\mathrm{width}\ [x_1 - y_2, y_1 - x_2]) \\
\implies & \frac{(y_1 - x_2) - (x_1 - y_2)}{2} \\
\implies & \frac{y_1 - x_1}{2} + \frac{y_2 - x_2}{2} \\
\implies & (\mathrm{width}\ [x_1, y_1]) + (\mathrm{width}\ [x_2, y_2])
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


## 练习 2.12
TODO

## 练习 2.13
TODO

## 练习 2.14
TODO

## 练习 2.15
TODO

## 练习 2.16
TODO

## 示例 表操作

```scheme
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

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
```


## 练习 2.17 定义 `last-pair`
```scheme
(define (reverse items)
  (define (iter xs ys)
    (if (null? xs)
        ys
        (iter (cdr xs) (cons (car xs) ys))))
  (iter items nil))
```

## 练习 2.18 定义 `reverse`
```scheme
(define (reverse items)
  (define (iter xs ys)
    (if (null? xs)
        ys
        (iter (cdr xs) (cons (car xs) ys))))
  (iter items nil))
```

## 练习 2.19 换零钱
```scheme
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
```

## 练习 2.20 `same-parity`
```scheme
(define (same-parity . items)
  (let ([parity (remainder (car items) 2)])
    (define (helper xs)
      (cond [(null? xs) '()]
            [(= (remainder (car xs) 2) parity)
             (cons (car xs) (helper (cdr xs)))]
            [else (helper (cdr xs))]))
    (helper items)))
```

## 实例 对象的映射
```scheme
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
```
## 更具有一般性的 `map` 过程
Scheme 提供的 `map` 以一个取 $n$ 个参数的过程和 $n$ 个表为参数，将这个过程应用于所有表的第一个元素，而后应用它们的第二个元素，如此下去，返回所有结果的表，例如：
```scheme
(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))

'(741 852 963)

(map (lambda (x y) (+ x (* 2 y)))
  (list 1 2 3)
  (list 4 5 6))

'(9 12 15)
```

## 练习 2.21 `square-list` 的实现
```scheme
(define (square-list items)
  (define (square x) (* x x))
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list* items)
  (define (square x) (* x x))
  (map square items))
```

## 练习 2.22 `square-list` 的迭代实现
```scheme
(iter '(1 2 3) '())
(iter '(2 3) '(1))
(iter '(3) '(1 2))
(iter '() '(1 2 3))
```

```scheme
(define (square-list** items)
  (define (square x) (* x x))
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list** '(1 2 3 4))
```

结果为
```
((((() . 1) . 4) . 9) . 16)
```
序列中的每一个序对的第二个元素应该指向下一个序对

## 练习 2.23 `for-each` 的实现
```scheme
(define (for-each f items)
  (if (null? (cdr items))
      (f (car items))
      (begin (f (car items)) (for-each f (cdr items)))))
```

## 实例 `count-leaves` 的实现
三种情况：
1. 空表的 `count-leaves` 是 0
2. 对于树 `x` 的 `count-leaves` 应该是 `(count-leaves (car x))` 和 `(count-leaves (cdr x))` 的和
3. 一个树叶的 `count-leaves` 是 1

```scheme
(define (count-leaves x)
  (cond [(null? x) 0]
        [(pair? x) (+ (count-leaves (car x)) (count-leaves (cdr x)))]
        [else 1]))
```

## 练习 2.24 `(list 1 (list 2 (list 3 4)))` 所代表的树
```scheme
(1 (2 (3 4)))
```

## 练习 2.25 给出下表中能够取出 7 的 `car` 和 `cdr` 组合
```scheme
(define a '(1 3 (5 7) 9))
(define b '((7)))
(define c '(1 (2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (cdr a)))))

(car (car b))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))
```

## 练习 2.26
```scheme
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
; (1 2 3 4 5 6)
(cons x y)
; ((1 2 3) 4 5 6)
(list x y)
; ((1 2 3) (4 5 6))
```

## 练习 2.27 `deep-reverse`
```scheme
(define (reverse items)
  (define (iter xs ys)
    (if (null? xs)
        ys
        (iter (cdr xs) (cons (car xs) ys))))
  (iter items nil))
```

```scheme
(define (deep-reverse items)
  (define (iter rest answer)
    (if (null? rest)
        answer
        (let ([cur (if (pair? (car rest))
                       (deep-reverse (car rest))
                       (car rest))])
          (iter (cdr rest) (cons cur answer)))))
  (iter items nil))
```

## 练习 2.28 `fringe`
```scheme
(define (fringe x)
  (cond [(null? x) '()]
        [(pair? x) (append (fringe (car x)) (fringe (cdr x)))]
        [else (list x)]))
```


## 练习 2.29 二叉活动体
```scheme
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
```

## 实例 对树的映射
```scheme
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
```

## 练习 2.30 定义 `square-tree`
```scheme
(define (square x) (* x x))

(define (square-tree tree)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (* tree tree)]
        [else (cons (square-tree (car tree))
                    (square-tree (cdr tree)))]))

(define (square-tree* tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
```

## 练习 2.31 `tree-map`
```scheme
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree** tree) (tree-map square tree))
```

## 练习 2.32 `求集合的所有子集`
```scheme
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (append (list (car s)) subset))
                          rest)))))
```

## 实例 计算值为奇数的叶子的平方和
```scheme
(define (sum-odd-squares tree)
  (cond [(null? tree) 0]
        [(not (pair? tree))
         (if (odd? tree) (square tree) 0)]
        [else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree)))]))
```

## 实例 构造出所有偶数的斐波那契数的一个表
```scheme
(define (even-fibs n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))

  (define (fib n) (fib-iter 1 0 n))

  (define (next k)
    (if (> k n)
        nil
        (let ([f (fib k)])
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))
```

## 实例 信号流辅助函数
```scheme
(define (filter predicate sequence)
  (cond [(null? sequence) nil]
        [(predicate (car sequence)) (cons (car sequence)
                                          (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (list tree)]
        [else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))]))
```

## 实例 重新实现的 `sum-odd-squares` 和 `even-fibs`
```scheme
(define (sum-odd-squares* tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs* n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))

  (define (fib n) (fib-iter 1 0 n))

  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))
```

## 练习 2.33 将一些基本的表操作看作累积的定义
```scheme
(define (map* p sequence)
  (accumulate (lambda (x rest) (cons (p x) rest)) nil sequence))

(define (append* seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length** sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
```

## 练习 2.34 Horner 规则
对于 $x$ 的某个给定值，求出一个多项式在 $x$ 的值，也可以形式化为一种累积。假定需要求下面的多项式：

$$ a_nx^n + a_{n-1}x^{n-1} + \cdots + a_1x + a_0 $$

采用著名的 Horner 规则，可以构造出下面的计算：

$$ (\cdots (a_nx + a_{n - 1})x + \cdots + a_1)x + a_0 $$

```scheme
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))
```

## 练习 2.35 将 `count-leaves` 重新定义为一个累积
```scheme
(define (count-leaves* t)
  (accumulate + 0 (map (lambda (root)
                         (cond [(null? root) 0]
                               [(not (pair? root)) 1]
                               [else (+ (count-leaves (car root))
                                        (count-leaves (cdr root)))])) t)))
```

## 练习 2.36 定义 `accumulate-n`
```scheme
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
```

## 练习 2.37 矩阵操作
```scheme
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)])
    (map (lambda (r)
           (map (lambda (c)
                  (dot-product r c)) cols)) m)))
```

## 练习 2.38 `fold-right`
```scheme
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(fold-right / 1 (list 1 2 3))
; 3/2

(fold-left / 1 (list 1 2 3))
; 1/6

(fold-right list nil (list 1 2 3))
'(1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
'(((() 1) 2) 3)
```
`op` 满足交换率时， `fold-right` 和 `fold-left` 在任何序列上都产生相同的结果

## 练习 2.39 分别使用 `fold-right` 和 `fold-left` 实现 `reverse`
```scheme
(define (reverse* sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse** sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
```