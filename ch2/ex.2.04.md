# `car`

```
(car (lambda (m) (m x y)))
((lambda (m) (m x y)) (lambda (p _) p))
((lambda (p _) p) x y)
x
```

# `cdr`
```
(car (lambda (m) (m x y)))
((lambda (m) (m x y)) (lambda (_ q) q))
((lambda (_ q) q) x y)
y
```