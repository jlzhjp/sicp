她说得对。

当一个变量的区间出现两次时，该系统会将这个变量视作两个独立变化的变量，从而导致出现与期望不符的结果，导致结果的区间范围变大。因此应该尽量避免一个变量在式子中重复出现。

# [Dependency Problem](https://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem)

> The so-called "dependency" problem is a major obstacle to the application of interval arithmetic. Although interval methods can determine the range of elementary arithmetic operations and functions very accurately, this is not always true with more complicated functions. If an interval occurs several times in a calculation using parameters, and each occurrence is taken independently, then this can lead to an unwanted expansion of the resulting intervals.
