# 强制进程按某种顺序运行
- Peter -> Paul -> Mary
    - 110 -> 90 -> 45
- Peter -> Mary -> Paul
    - 110 -> 55 -> 35
- Paul -> Peter -> Mary
    - 80 -> 90 -> 45
- Paul -> Mary -> Peter
    - 80 -> 40 -> 50
- Mary -> Peter -> Paul
    - 50 -> 60 -> 40
- Mary -> Paul -> Peter
    - 50 -> 30 -> 40

所有可能的值：35、40、45、50

# 允许进程交错执行

> https://mk12.github.io/sicp/exercise/3/4.html#ex3.38
> If the system allows the processes to be interleaved, you could also get results equivalent to leaving out one or more of the assignments, where the new value is overwritten before being read.