# 原版

```racket
(flatmap (lambda (rest-of-queens)
           ; new-row: proposed row in which
           ; to place the queen for the kth column
           (map (lambda (new-row)
                  (adjoin-position new-row k rest-of-queens))
                (enumerate-interval 1 board-size)))
         (queen-cols (- k 1)))
```

# Louis Reasoner

```racket
(flatmap (lambda (new-row)
           (map (lambda (rest-of-queens)
                  (adjoin-position new-row k rest-of-queens))
                (queen-cols (- k 1))))
         (enumerate-interval 1 board-size))
```
Louis 的版本每次加入第 k 列的一个新皇后的位置时，都要重新计算 k - 1 列的所有皇后的位置，造成了大量的重复计算，因此运行地非常慢。

# 估计 Louis 程序所需的时间

TODO

https://wernerdegroot.wordpress.com/2015/08/01/sicp-exercise-2-43/