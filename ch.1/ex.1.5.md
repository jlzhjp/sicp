defines the following two procedures

```racket
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
```

evaluates the expression

```racket
(test 0 (p))
```

- 对应用序：先求值参数，p 为无限递归函数，因此程序不会有输出
- 对正则序：先传递参数，需要时再求值，(= x 0) 的条件满足，p 不会被执行，程序输出 0