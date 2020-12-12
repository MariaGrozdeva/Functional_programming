#lang racket/base

(define (paths tree)
  (if (null? tree)
      '()
      (if (and (null? (cadr tree)) (null? (caddr tree)))
          (list (list (car tree)))
          (map (lambda (x) (cons (car tree) x))
               (append (paths (cadr tree)) (paths (caddr tree)))) ) )
  )