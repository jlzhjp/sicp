```racket
(width-interval (add-interval ('interval a b) ('interval c d)))
(width-interval ('interval (+ a c) (+ b d)))
(/ (- (+ b d) (+ a c)) 2)
(+ (/ (- b a) 2) (/ (- d c) 2))
(+ (width-interval ('interval a b) (width-interval ('interval c d))))
```

```racket
(width-interval (sub-interval ('interval a b) ('interval c d)))
(width-interval ('interval (- a d) (- b c)))
(/ (- (- b c) (- a d)) 2)
(+ (/ (- b a) 2) (/ (- d c) 2))
(+ (width-interval ('interval a b)) (width-interval ('interval c d)))
```