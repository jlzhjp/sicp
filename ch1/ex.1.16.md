$$
b^n = \begin{cases}
    (b^{\frac n 2})^2, &\mathtt{(even?\ n)} \\
    b(b^{n - 1}), &\mathtt{(odd?\ n)} \\
    1, & \mathtt{(=\ n\ 0)}
\end{cases}
$$

$$
a \cdot b^n = \begin{cases}
    a \cdot (b^2)^{\frac n 2}, &\mathtt{(even?\ n)} \\
    (ab) \cdot b^{n - 1}, &\mathtt{(odd?\ n)} \\
    a, &\mathtt{(=\ n\ 0)}
\end{cases}
$$