#lang racket/base

(define (group-consecutive l)
  (foldr
   (lambda (x y)
     (if (or (null? y) (not (= (car (car y)) x)))
         (cons (list x) y)
         (cons (cons x (car y)) (cdr y)) ) )
     '()
     l )
  )
; --> '((1 1) (2) (3 3))

(define (look-and-say l)
  (define ll (group-consecutive l))
  (foldr append '() (foldr
                     (lambda (x y) (cons (list (length x) (car x)) y)) '() ll) )
  )
; --> '(2 1 1 2 2 3)