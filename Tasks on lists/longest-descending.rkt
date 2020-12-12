#lang racket/base

(define (longest-descending-prefix l)
  (define (longest-descending-prefix-helper l lastDigit)
    (if (or (null? l) (< lastDigit (car l)))
        '()
        (cons (car l) (longest-descending-prefix-helper (cdr l) (car l))) )
    )
  (longest-descending-prefix-helper l +inf.0)
  )

(define (descending-sublists l)
  (if (null? l)
      '()
      (cons (longest-descending-prefix l) (descending-sublists (cdr l))) )
  )

(define (longest-descending l)
  (define (longest-descending-helper l)
    (foldr (lambda (x y) (if (>= (length x) (length y)) x y)) '() l)
    )
  (define desc-subl (descending-sublists l))
  (longest-descending-helper desc-subl)
  )
