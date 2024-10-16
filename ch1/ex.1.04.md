```racket
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
```

- 如果 b > 0，则将参数 a b 应用到函数 + 上
- 如果 b < 0，则将参数 a b 应用到函数 - 上