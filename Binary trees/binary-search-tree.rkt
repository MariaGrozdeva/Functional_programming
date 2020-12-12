#lang racket/base

(define (binary-search-tree? tree)
  (define (binary-search-tree-helper tree min max)
    (if (null? tree)
        #t
        (if (or (> (car tree) max) (< (car tree) min))
            #f
            (and (binary-search-tree-helper (cadr tree) min (- (car tree) 1)) (binary-search-tree-helper (caddr tree) (+ (car tree) 1) max)) ) )
        )
    (binary-search-tree-helper tree -inf.0 +inf.0)
    )