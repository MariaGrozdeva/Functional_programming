#lang racket/base

(define (contains? tree path)
  (if (null? path)
      #t
      (if (null? tree)
          #f
          (if (= (car path) (car tree))
              (or (contains? (cadr tree) (cdr path)) (contains? (caddr tree) (cdr path)))
              (or (contains? (cadr tree) path) (contains? (caddr tree) path)) ) ) )
  )