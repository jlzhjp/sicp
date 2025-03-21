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

Alyssa P. Hacker's version

```rkt
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
```

`expmod` 是一个让操作数先剧烈变大再剧烈变小的过程，如果采用简单的实现可能会导致溢出，原先的实现能够将操作数保持在一个较小的范围之内