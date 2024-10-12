$angle = a$ 时，需要迭代 $n$ 次

$$
\begin{aligned}
\frac a {3^n} &\le 0.1 \\
a &\le 0.1 \times 3^n \\
3^n &\ge 10 a \\
n \cdot \log 3 &\ge \log 10a \\
n &\le \frac {\log {10 a}} {\log 3}
\end{aligned}
$$

$n$ 的增长阶为 $\Theta(\log a)$