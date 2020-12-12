## 1. Lists
## 2. Binary trees:
Представяме двоичните дървета със следната конструкция:
_**(root left-tree right-tree)**_
```diff
- * Tree
(define tree
  '(1
      (2 () ())
      (3
          (4 () ())
          (5 () ()))))

```diff
+ * Binary search tree
(define binary-search-tree
  '(3
      (1
          ()
          (2 () ()))
      (4
          ()
          (5 () ()))))
