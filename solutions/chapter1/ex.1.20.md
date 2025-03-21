# Normal Order

```racket
(gcd 206 40)
(gcd 40 (remainder 206 40))
; remainder call: 1 (if)
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; remainder call: 3 (if)
(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; remainder call: 7 (if)
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40))
                (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; remainder call: 14 (if)
; remainder call: 18 (evaluate a)
```

# Applicative Order

```
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6) ; remainder call: 1
(gcd 6 (remainder 40 6))
(gcd 6 4) ; remainder call: 2
(gcd 4 (remainder 6 4))
(gcd 4 2) ; remainder call: 3
(gcd 2 (remainder 4 2))
(gcd 2 0) ; remainder call: 4
2
```