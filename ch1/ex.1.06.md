```racket
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
```

Scheme 默认采用应用序求值，因此对于 new-if 来说，无论 predicate 参数的值是什么，then-clause 和 else-clause 都会被执行。

```racket
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
```

在以上程序中， `sqrt-iter` 函数会不断被执行，导致程序不产生任何输出