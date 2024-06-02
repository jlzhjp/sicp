# 第 2 章 构造数据抽象

## 练习 2.1 改进 `make-rat` 的实现

```rkt
(define (make-rat n d)
  (let [(divisor (gcd n d))]
    (let [(n* (/ n divisor))
          (d* (/ d divisor))]
      (cond [(> d* 0) (cons n* d*)]
            [(< d* 0) (cons (- n*) (- d*))]))))
```