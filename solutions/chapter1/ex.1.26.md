```racket
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m)]
        [else
         (remainder (* base (expmod base (- exp 1) m))
                    m)]))
```

Louis Reasoner's version

```rkt
(define (expmod base exp m)
  (cond [(= exp 0) 1]
        [(even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m)]
        [else
         (remainder (* base (expmod base (- exp 1) m))
                    m)]))
```

相比原来的版本，该版本的 `expmod` 每次执行 `(even? exp)` 分支时都比原来多进行了一次递归。因此时间复杂度变为

$$
\Theta(2^{\log n}) = \Theta(n)
$$