#lang racket/base

(define (is-unique-in-list el l)
  (define (is-unique-in-list-helper el l)
  (if (null? l)
      0
      (if (= (car l) el)
          (+ 1 (is-unique-in-list-helper el (cdr l)))
          (is-unique-in-list-helper el (cdr l)) ) )
    )
  (= (is-unique-in-list-helper el l) 1)
  )

(define (max-in-list l)
  (define (max-in-list-helper l)
    (foldr (lambda (x y) (if (and (is-unique-in-list x l) (> x y)) x y)) -inf.0 l)
    )
  (if (eq? (max-in-list-helper l) -inf.0)
      #f
      (max-in-list-helper l) )
  )

(define (max-unique ll)
  (define (max-unique-helper ll)
    (foldr
     (lambda (x y)
       (if (and (not (eq? (max-in-list x) #f)) (> (max-in-list x) y))
           (max-in-list x) y) )
     -inf.0
     ll)
    )
  (if (eq? (max-unique-helper ll) -inf.0)
      #f
      (max-unique-helper ll) )
  )