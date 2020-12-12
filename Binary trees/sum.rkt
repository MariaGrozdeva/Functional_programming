#lang racket/base

(define (sum tree)
  (if (null? tree)
      0
      (+ (car tree) (sum (cadr tree)) (sum (caddr tree))) )
  )