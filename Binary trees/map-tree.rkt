#lang racket/base

(define (map-tree function tree)
  (if (null? tree)
      '()
      (list (function (car tree)) (map-tree function (cadr tree)) (map-tree function (caddr tree))) )
  )