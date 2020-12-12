## 1. Lists
## 2. Binary trees:
Представяме двоичните дървета със следната конструкция:
_**(root left-tree right-tree)**_
```diff
- Binary tree
(define tree
  '(1
      (2 () ())
      (3
          (4 () ())
          (5 () ()))))

+ Binary search tree
(define binary-search-tree
  '(3
      (1
          ()
          (2 () ()))
      (4
          ()
          (5 () ()))))

! Heap tree
(define heap-tree
    '(5
      (6
       (7 () ())
       (8 () ()))
      (9
       ()
       (11
        (12 () ())
        ()))))
