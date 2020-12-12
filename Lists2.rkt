#lang racket

; Задача 0: Да се дефинира функция (take n l), която връща първите n елемента на списъка l
(define (take n l)
  (if (or (empty? l) (= n 0))
      null
      (cons (car l) (take (- n 1) (cdr l)))
      )
  )

; Задача 1: Да се дефинира функция (drop n l), която връща списък без първите n елемента на списъка l
(define (drop n l)
  (if (or (empty? l) (= n 0))
      l
      (drop (- n 1) (cdr l))
      )
  )

; Задача 2: Да се дефинира функция (is-palindrome? l), която връща #t ако списъка l е палиндром и #f в противен случай
(define (my-reverse l)
  (if (empty? l)
      null
      (append (my-reverse (cdr l)) (list (car l))) )
  )
(define (is-palindrome? l)
  (equal? l (my-reverse l))
  )

; Задача 3: Да се дефинира функция (flatten ls), която приема списък от списъци ls и връща списък, съставен от елементите на списъците в ls
(define (flatten ls)
  (if (empty? ls)
      ls
      (append (car ls) (flatten (cdr ls)))
      )
  )
  ;(flatten (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

; Задача 3*: Да се дефинира функция (flatten-deep lss), която приема списък от произволни списъци lss (елементите на lss също може да са списъци от списъци, техните елементи също, и т.н.) и връща като резултат всички принадлежащи елементи на тези списъци в единствен списъ.

; Задача 4: Да се дефинира функция (dot-product v1 v2), която връща като резултат скаларното произведение на векторите v1 и v2 (представени като списъци)
(define (dot-product v1 v2)
  (if (empty? v1)
      0
      (+ (* (car v1) (car v2)) (dot-product (cdr v1) (cdr v2)))
      )
  )

; Задача 5: Да се дефинира функция (sum-matrix m1 m2), която връща като резултат матрицата равна на сумата на матриците m1 и m2 (представени като списъци от списъци)
(define (sum-lists l1 l2)
  (if (empty? l1)
      null
      (append (list (+ (car l1) (car l2))) (sum-lists (cdr l1) (cdr l2)))
      )
  )
(define (sum-matrix m1 m2)
  (if (empty? m1)
      null
      (append (sum-lists (car m1) (car m2)) (sum-matrix (cdr m1) (cdr m2)) )
      )
  )
;(sum-matrix '((1 2) (3 4)) '((5 6) (7 8)))

; Задача 5*: Да се дефинира функция (matrix-mult m1 m2), която връща като резултат произведението на матриците m1 и m2

; Задача 6: Да се дефинира функция (transpose-matrix m), която връща транспсонираната матрица на m
(define (transpose-matrix m)
  (if (empty? (car m))
      null
      (cons (map car m) (transpose-matrix (map cdr m)) )
      )
  )
;(transpose-matrix (list '(1 2 3) '(4 5 6) '(7 8 9)))