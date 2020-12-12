#lang racket/base

(define (map-into-itself? l f)
  (define (map-into-itself-helper new)
    (if (null? new)
        #t
        (if (member (car new) l)
            (map-into-itself-helper (cdr new))
            #f ) )
    )
  (define newL (map f l))
  (map-into-itself-helper newL)
  )
; (map-into-itself? '(0 1 4 6) (lambda (x) (remainder x 5))) --> #t
; (map-into-itself? '(0 1 4 6) (lambda (x) (remainder x 4))) --> #f

(define (ordered-pairs l)
  (define (ordered-pairs-helper l1 l2)
    (if (null? l1)
        '()
        (if (null? l2)
            (ordered-pairs-helper (cdr l1) l)
            (cons (cons (car l1) (car l2)) (ordered-pairs-helper l1 (cdr l2))) ) )
    )
  (ordered-pairs-helper l l)
  )
; (ordered-pairs '(1 3)) --> '((1 . 1) (1 . 3) (3 . 1) (3 . 3))

(define (closed-under-operation l op f)
  (foldr
   (lambda (x y)
     (if (= (f (op (car x) (cdr x))) (op (f (car x)) (f (cdr x))))
         y
         #f ) )
     #t
     (ordered-pairs l) )
  )
; (closed-under-operation '(0 1 4 6) + (lambda (x) (remainder x 3))) --> #t
; (closed-under-operation '(0 1 4 6) + (lambda (x) (remainder x 5))) --> #f

(define (is-em? l op f)
  (and (map-into-itself? l f) (closed-under-operation l op f) )
  )