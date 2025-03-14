|                    | 不含重复的表 | 含重复的表        |
| ------------------ | ------------ | ----------------- |
| `element-of-set?`  | $O(n)$       | $O(kn)$           |
| `adjoin-set`       | $O(n)$       | $O(1)$            |
| `union-set`        | $O(mn)$      | $O(kn)$           |
| `intersection-set` | $O(mn)$      | $O((k_1m)(k_2n))$ |

$k$ 为表中实际元素数量与集合中元素数量的比值。当 $k$ 比较小时，使用含重复元素的表来表示集合比较高效，当 $k$ 比较大时，使用不含重复元素的表来表示集合比较高效。

当插入操作较多，而取交集、并集、查找等操作较少时，使用含重复元素的表来表示集合可能能够取得较高的效率；当插入操作较少，而取交集、并集、查找等操作较多时，使用不含重复元素的表来表示集合可能取得较高的效率。

一般情况下， $k$ 值的大小难以预测，因此一般应采用不含重复元素的表来表示集合。