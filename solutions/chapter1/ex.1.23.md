|           Number | Fast | Slow | Ratio |
| ---------------: | ---- | ---- | ----- |
|    1000000000039 | 2    | 3    | 0.67  |
|    1000000000061 | 2    | 3    | 0.67  |
|    1000000000063 | 3    | 3    | 1     |
|   10000000000037 | 7    | 10   | 0.7   |
|   10000000000051 | 7    | 11   | 0.64  |
|   10000000000099 | 7    | 11   | 0.64  |
|  100000000000031 | 23   | 32   | 0.72  |
|  100000000000067 | 23   | 32   | 0.72  |
|  100000000000097 | 23   | 32   | 0.72  |
| 1000000000000037 | 72   | 100  | 0.72  |
| 1000000000000091 | 72   | 100  | 0.72  |
| 1000000000000159 | 72   | 99   | 0.73  |

# Explanation
函数调用的开销和多出的 `if` 判断抵消了优化

TODO: 实验证明