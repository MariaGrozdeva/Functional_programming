## 1. Lists
## 2. Binary trees:
Представяме двоичните дървета със следната конструкция:
*(root left-tree right-tree)*

```diff
-(define example-tree
```diff
  '(1
      (2 () ())
      (3
          (4 () ())
          (5 () ()))))
