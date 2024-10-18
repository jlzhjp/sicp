```racket
(square-list '(1 2 3))
(iter '(1 2 3) '())
(iter '(2 3) '(1))
(iter '(3) '(4 1))
(iter '() '(9 4 1))
'(9 4 1)
```

```racket
(square-list* '(1 2 3))
(iter '(1 2 3) '())
(iter '(2 3) '(() . 1))
(iter '(3) '((() . 1) . 2))
(iter '() '(((() . 1) . 2) . 3))
```