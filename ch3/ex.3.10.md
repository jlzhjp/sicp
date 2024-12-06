```scheme
(define (make-withdraw initial-amount)
  (let ([balance initial-amount])
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))
```
1. 在全局环境中定义 `make-withdraw`， `make-withdraw` 的值可看作一个 $\lambda$ 表达式
2. 对 `make-withdraw` 的 $\lambda$ 表达式求值，得到一个序对，序对的 `Env` 部分指向 `make_withdraw` 所在的全局环境， `Body` 部分指向 $\lambda$ 表达式的代码
3. 在参数 `initial-amount = 100` 上应用 `make-withdraw`，建立一个新框架 `Frame1`，`Frame1` 的外围环境是全局环境，形式参数 `init-amount` 约束于对应的实际参数 `100`，将该框架称为环境 `E1`
4. `make-withdraw` 的的过程体是一个 $\lambda$ 表达式，在环境 `E1` 下求值，得到一个序对（`<let-lambda>` 指向的序对）,其 `Env` 部分指向当前环境 `Frame1`
5. 在参数 `balance = 100` 上应用 `<let-lambda>` ，得到一个新框架 `Frame2` ，并绑定 `Frame1` 为外围环境，并约束 `balance` 的值为 `100`，称该框架为环境 `E2`
6. `<let-lambda>` 的过程体为一个 $\lambda$ 表达式，在环境 `E2` 下求值，得到一个序对，其 `Env` 部分指向当前环境 `Frame2`
7. 将得到的序对作为返回值返回，在全局环境中将 `W1` 绑定到返回值上
8. 在参数 `amount = 50` 上应用 `W1` ，建立一个新框架 `Frame3` ，将 `Frame3` 的的外围环境绑定为 `Frame2`，将该框架称为环境 `E3`
9. 在环境 `E3` 下执行函数体，顺着当前环境的框架链条向上寻找 `balance`，在 `Frame2` 中找到， `(>= balance amount)` 的条件满足，将 `Frame2` 中的 `balance` 更新为 `50`
10. `W2` 的获取过程与上述类似，注意在应用过程时要建立新的框架
