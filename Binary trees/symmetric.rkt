#lang racket/base

(define (reverse tree)
  (if (null? tree)
      null
      (list (car tree) (reverse (caddr tree)) (reverse (cadr tree))) )
  )

(define (symmetric? tree)
  (define (symmetric-helper tree-left tree-right)
    (if (and (null? tree-left) (null? tree-right))
        #t
        (if (or (and (null? tree-left) (not (null? tree-right))) (and (null? tree-right) (not (null? tree-left))))
            #f
            (and (car tree-left) (car tree-right)
                 (symmetric-helper (cadr tree-left) (cadr tree-right)) (symmetric-helper (caddr tree-left) (caddr tree-right))) ) )
    )
  (symmetric-helper (cadr tree) (reverse (caddr tree)))
  )