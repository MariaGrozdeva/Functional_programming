#lang racket/base

(define (level tree i)
  (if (not (null? tree))
      (if (= i 1)
          (list (car tree))
          (append (level (cadr tree) (- i 1)) (level (caddr tree) (- i 1))) )
      '()
      )
  )