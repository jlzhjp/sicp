```racket
(define (A x y)
  (cond [(= y 0) 0]
        [(= x 0) (* 2 y)]
        [(= y 1) 2]
        [else (A (- x 1)
                  (A x (- y 1)))]))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
```

```
(f 0) => (A 0 0) => 0

(f n)
=> (A 0 n)
=> (* 2 n)


(g 0) => (A 1 0) => 0
(g 1) => (A 1 1) => 2

(g n)
=> (A 1 n)
=> (A 0 (A 1 (- n 1)))
=> (* 2 (A 1 (- n 1)))
=> (* 2 (g (- n 1)))
=> ...

(g n) => (expt 2 n)


(h 0) => (A 2 0) => 0
(h 1) => (A 2 1) => 2

(h n)
=> (A 2 n)
=> (A 1 (A 2 (- n 1)))
=> (g (h (- n 1)))
=> (g (g (h (- n 2))))
=> ...

(h n) => apply g to 2 (- n 1) times
```

$$
f(n) = 2n
$$

$$
g(n) = \begin{cases}
0, & n = 0 \\
2^n, & n > 0
\end{cases}
$$

$$
h(n) = \begin{cases}
0, & n = 0 \\
g^{n-1}(2) = \underbrace{2^{\cdot^{\cdot^{\cdot^2}}}}_n & n > 0
\end{cases}
$$