# 第 1 章 构造过程抽象

## 练习 1.2 表达式前缀形式

```rkt
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
```

## 练习 1.3 两个较大数的和

```rkt
(define (sum-of-max-two a b c)
  (if (>= a b)
      (if (>= b c) (+ a b) (+ a c))
      (if (> a c) (+ b a) (+ b c))))
```

## 实例 牛顿法求平方根

```rkt
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))
```

## 练习 1.8 牛顿法求立方根
```rkt
(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (cube-root x) (cube-root-iter 1.0 x))
```

## 实例 内部定义与块结构

```rkt
(define (sqrt x)
  (define (square x) (* x x))
  (define (average x y) (/ (+ x y) 2))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))
```

## 练习1.9 对比递归和迭代产生的计算过程

```rkt
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9
```

## 练习1.10 Ackermann 函数

TODO

## 实例 斐波那契数列的树形递归与迭代实现

```rkt
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
```

```rkt
(define (fib* n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))

  (fib-iter 1 0 n))
```

## 实例 换零钱方式的统计

```rkt
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
```
## 练习 1.11 使用递归和迭代计算函数$f$

```rkt
; n < 3 => f(n) = n
; n >= 3 => f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3)

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))


(define (f* n)
  (define (f*-iter i f*-n-one f*-n-two f*-n-three)
    (if (> i n)
        f*-n-one
        (f*-iter (+ i 1)
                 (+ f*-n-one (* 2 f*-n-two) (* 3 f*-n-three))
                 f*-n-one
                 f*-n-two)))
  (cond ((< n 3) n)
        (else
         (f*-iter 3 2 1 0))))
```

## 练习 1.12 计算帕斯卡三角形
```rkt
(define (pascal-triangle n-row n-col)
  (if (= n-row 1)
      (if (= n-col 1) 1 0)
      (if (and (> n-col 0) (<= n-col n-row))
          (+ (pascal-triangle (- n-row 1) (- n-col 1))
             (pascal-triangle (- n-row 1) n-col))
          0)))

(define (print-pascal-triangle n)
  (define (iter i)
    (define (iter* j)
      (cond ((> j i) #f)
            (else (display (pascal-triangle i j))
                  (display " ")
                  (iter* (+ j 1)))))

    (cond ((> i n) #f)
          (else (iter* 1)
                (newline)
                (iter (+ i 1)))))
  (iter 1))
```
## 练习 1.13证明 $Fib(n)$ 是最接近 $\Phi/\sqrt{5}$ 的整数
TODO

## 练习 1.14
$$
2^n
$$

## 练习1.15
TODO

## 实例：求幂

```rkt
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt* b n)
  (define (iter b counter product)
    (if (= counter 0)
        product
        (iter b (- counter 1) (* product b))))
  (iter b n 1))

(define (fast-expt* b n)
  (define (fast-expt-iter a b n)
    (cond [(= n 0) a]
          [(even? n) (fast-expt-iter a (square b) (/ n 2))]
          [else (fast-expt-iter (* a b) b (- n 1))]))

  (fast-expt-iter 1 b n))
```

## 练习 1.16 将快速求幂函数修改为迭代实现
$$
a(b^n) = a(b^\frac{n}{2})^2 = a(b^2)^\frac{n}{2} = a'b'^{n'}
$$

$$
\begin{aligned}
a' &= a \\
b' &= b^2 \\
n' &= n/2
\end{aligned}
$$

$$
a(b^n) = a b b^{n - 1} = (a b)b^{n - 1} = a'b'^{n'}
$$

$$
\begin{aligned}
a' &= a b \\
b' &= b \\
n' &= n - 1
\end{aligned}
$$

> [!NOTE]
> 定义一个不变量，要求它在状态之间保持不变，这一技术是思考迭代算法设计问题时的一种非常强有力的方法

```rkt
(define (fast-expt* b n)
  (define (fast-expt-iter a b n)
    (cond [(= n 0) a]
          [(even? n) (fast-expt-iter a (square b) (/ n 2))]
          [else (fast-expt-iter (* a b) b (- n 1))]))

  (fast-expt-iter 1 b n))
```

## 练习 1.17 使用累加法求乘积
$$
a \times b = a + a \times (b - 1)
$$
```rkt
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (fast-mul a b)
  (cond [(= b 1) a]
        [(even? b) (double (fast-mul a (halve b)))]
        [else (+ a (fast-mul a (- b 1)))]))
```

## 练习 1.18 将 1.17 改为迭代实现/俄罗斯农民
`(even? b)`

$$
a \times b + c = (a \times 2) \times (b \div 2) + c
$$

`(odd? b)`

$$
a \times b + c = a \times (b - 1) + (c + a)
$$

```rkt
(define (fast-mul* a b)
  (define (fast-mul-iter a b c)
    (cond [(= b 0) c]
          [(even? b) (fast-mul-iter (double a) (halve b) c)]
          [else (fast-mul-iter a (- b 1) (+ c a))]))
  (fast-mul-iter a b 0))

```

## 练习1.19
$$
\begin{aligned}
a &\gets b q + a q + a p \\
b &\gets b p + a q
\end{aligned}
$$

$$
\begin{aligned}
a' &= b q + a q + a p \\
b' &= b p + a q \\
a'' &= b' q + a' q + a' p \\
    &= (b p + a q) q + (b q + a q + a p) q + (b q + a q + a p) p \\
    &= b p q + a q^2 + b q^2 + a q^2 + a p q + b p q + a p q + a p^2 \\
    &= (p^2 + 2 q^2 + 2 p q)a + (q^2+2 p q)b \\
    &= b(q^2 + 2 p q) + a(q^2 + 2 p q) + a(p^2 + q^2) \\
b'' &= b'p + a'q \\
    &= (b p + a q)p + (b q + a q + a p)q \\
    &= b p^2 + a p q + b q^2 + a q^2 + a p q \\
    &= (q^2 + 2 p q)a + (p ^ 2 + q^2)b \\
    &= b(p^2 + q^2) + a(q^2 + 2p q)
\end{aligned}
$$

$$
\begin{aligned}
p' &= p^2 + q^2 \\
q' &= q^2 + 2 p q
\end{aligned}
$$

```rkt
(define (fib n)
  (define (fib-iter a b p q count)
    (cond [(= count 0) b]
          [(even? count) (fib-iter a
                                   b
                                   (+ (square p) (square q))
                                   (+ (square q) (* 2 p q))
                                   (/ count 2))]
          [else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1))]))
  (fib-iter 1 0 0 1 n))
```

## 练习1.20 使用正则序和应用序时 `remainder` 的调用次数

Normal Order: fully expand and then reduce
TODO

Applicative Order: evaluate the arguments and then apply
TODO

## 实例 素数检测
$$
\Theta (\sqrt{n})
$$
```
(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a  b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond [(> (square test-divisor) n) n]
        [(divides? test-divisor n) test-divisor]
        [else (find-divisor n (+ test-divisor 1))]))

(define (prime? n)
  (= n (smallest-divisor n)))
```

> [!NOTE]
> **费马小定理**：如果 $n$ 是一个素数， $a$ 是小于 $n$ 的任意正整数，那么 $a$ 的 $n$ 次方与 $a$ 模同余。（两个数称为是模 $n$ 同余，如果它们除以 $n$ 的余数相同。数 $a$ 除以 $n$ 的余数称为 $a$ 取模 $n$ 的余数，或简称为 $a$ 取模 $n$ ）

对于指数 $e > 1$ 的情况，所采用的规约方式基于以下事实：
对任意的 $x, y, m$ ，有

$$
x\ y\ mod\ m = (x\ mod\ m)(y\ mod\ m)\ mod\ m
$$

例如：在$e$是偶数时，

$$
b^e\ mod\ m = (b^(e/2)\ mod\ m)^2\ mod\ m
$$

```rkt
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m)]
        [else
         (remainder (* base (expmod base (- exp 1) m))
                    m)]))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond [(= times 0) true]
        [(fermat-test n) (fast-prime? n (- times 1))]
        [else false]))
```

## 练习 1.21 找出 199、1999、19999的最小公因子
```rkt
(smallest-divisor 199)
; 199
(smallest-divisor 1999)
; 1999
(smallest-divisor 19999)
; 7
```
## 练习 1.22 找出指定范围内最小的素数
```rkt
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time)) #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)

(define (search-for-primes n)
  (if (even? n)
      (search-for-primes (+ n 1))
      (if (not (timed-prime-test n))
          (search-for-primes (+ n 2)))) )

(search-for-primes 100000)
(search-for-primes 1000000)
(search-for-primes 10000000)

; 100001
; 100003 *** 1
; 1000001
; 1000003 *** 4
; 10000001
; 10000003
; 10000005
; 10000007
; 10000009
; 10000011
; 10000013
; 10000015
; 10000017
; 10000019 *** 11
```

## 练习 1.23
TODO

## 练习 1.24
TODO

## 练习 1.25
溢出

## 练习1.26
$$
2^{\log_2{n}} = n
$$

## 练习1.27 证明 Carmichael 数能够骗过费马检查

```rkt
(define (full-fermat-test n)
  (define (full-fermat-test-iter a)
    (cond [(= a 1) #t]
          [(not (= (expmod a n n) (remainder a n))) #f]
          [else (full-fermat-test-iter (- a 1))]))
  (full-fermat-test-iter (- n 1)))

(define (run-full-fermat-test-on-carmichael-numbers)
  (display (full-fermat-test 561))
  (newline)
  (display (full-fermat-test 1105))
  (newline)
  (display (full-fermat-test 1729))
  (newline)
  (display (full-fermat-test 2465))
  (newline)
  (display (full-fermat-test 2821))
  (newline)
  (display (full-fermat-test 6601))
  (newline))
```

## 练习 1.28
TODO

## 练习 1.29 使用辛普森规则求积分
```rkt
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
```
```rkt
(simpson-integral cube 0 1 100)
; 0.25
(simpson-intergral cube 0 1 1000)
; 0.25
```

## 练习 1.30 基于迭代的求和函数
```rkt
(define (sum* term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (+ a 1) (+ (term a) result))))
  (iter a 0))
```

## 练习 1.31 求$\pi$值
$$
\prod_{n=1}^{\infty}\left(\frac{2n}{2n - 1} \cdot \frac{2n}{2n + 1}\right)
$$

```rkt
(define (product factor a next b)
  (if (> a b)
      1
      (* (factor a) (product factor (next a) next b))))

(define (factorial x)
  (product identity 1 inc x))

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
```

## 练习 1.32 使用`accumulate`实现`sum`和`product`
```rkt
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
```

## 练习 1.33 `accumulate` + `filter`
```rkt
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
```