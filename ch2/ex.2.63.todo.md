# 主定理
假设有递归式
$$
T(n) = a T (\frac n b) + f(n),\ \text{其中}\ a \ge 1, b > 1
$$

其中， $n$ 为问题规模， $a$ 为递归的子问题数量， $\frac n b$ 为每个子问题的规模（假设每个子问题的规模基本一样， $f(n)$ 为递归以外进行的计算工作

## 情形一

如果存在常数 $ \epsilon > 0 $，有

$$
f(n) = O(n^{\log_b a - \epsilon})
$$

则

$$
T(n) = \Theta(n^{\log_b a})
$$

## 情形二

如果存在常数 $ \epsilon \ge 0 $ ，有

$$
f(n) = \Theta(n^{\log_b a} \log^\epsilon n)
$$

则

$$
T(n) = \Theta(n^{\log_b a - \epsilon})
$$

# `tree->list-1`

TDOO

# `tree->list-2`

TODO
