Suppose we define the procedure

```racket
(define (f g) (g 2))
```

Then we have

```racket
(f square)
4
(f (lambda (z) (* z (+ z 1))))
6
```

What happens if we (perversely) ask the interpreter to evaluate the combination `(f f)` ?

```
(f f)
(f 2)
(2 2)
; 2 is not a procedure
```