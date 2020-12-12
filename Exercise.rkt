#lang racket

; Задача 0: Да се дефинира функция (solve-quadratic a b c), която връща като резултат решенията на квадратното уравнение ax^2 + bx + c = 0
(define (solve-quadratic a b c)
  (define discriminant (- (* b b) (* 4 (* a c))) )
  (define x1 (/ (+ (- b) (sqrt discriminant)) (* 2 a)) )
  (define x2 (/ (- (- b) (sqrt discriminant)) (* 2 a)) )
  (if (< discriminant 0)
      null
      (if (= discriminant 0)
          (list x1)
          (list x1 x2)
          )
      )
  )


; Задача 1: Да се дефинира функция (mid p1 p2), която намира средата на отсечката P1P2 (p1, p2 - точки в равнина)
(define (mid p1 p2)
  (define x1 (car p1) )
  (define y1 (cdr p1) )
  (define x2 (car p2) )
  (define y2 (cdr p2) )
  (list (/ (+ x1 x2) 2) (/ (+ y1 y2) 2) )
  )


; Задача 2: Да се дефинира функция (my-length l), която връща дължината на списъка l
(define (my-length l)
  (define (get-length list length)
    (if (empty? list)
        length
        (get-length (cdr list) (+ length 1))
        )
    )
    (get-length l 0)
  )
; WITH FOLD
(define (my-length-fold l)
  (foldl (lambda (x y) (+ y 1)) 0 l)
  )


; Задача 3: Да се дефинира функция (my-reverse l), която връща списъка, съдържащ елементите на l с обърната последователност
(define (my-reverse l)
  (if (empty? l)
      l
      (append (my-reverse (cdr l)) (list (car l)))
      )
  )
; WITH FOLD
(define (my-reverse-fold l)
  (foldl (lambda (x y) (append (list x) y))  '() l)
  )


; Задача 4: Да се дефинира функция (my-map f l), която връща списък с елементи y = f(x) при x - елементи на l
(define (my-map f l)
  (if (empty? l)
      null
      (cons (f (car l)) (my-map f (cdr l)) )
      )
  )
; WITH FOLD
(define (my-map-fold f l)
  (foldr (lambda (x y) (cons (f x) y) ) null l)
  )


; Задача 5: Да се дефинира функция (take-elem l n), която връща n-тия елемент на списъка l ((take-elem l 1) <=> (car l))
(define (take-elem l n)
  (if (empty? l)
      #f
      (if (= n 1)
          (car l)
          (take-elem (cdr l) (- n 1))
          )
      )
  )


; Задача 6: Да се дефинира функция (quick-sort l), която имплементира алгоритъма quicksort)
(define (quick-sort l)
  (if (<= (length l) 1)
      l
      (let*
      (
      (pivot (take-elem l (quotient (length l) 2)) )
      (smaller (lambda (x) (< x pivot)) )
      (bigger (lambda (x) (> x pivot)) )
      )
      (append (quick-sort (filter smaller l)) (list pivot) (quick-sort (filter bigger l))) )
        )
      )


; Задача 0: Да се дефинира функция (take n l), която връща първите n елемента на списъка l
(define (take n l)
  (if (or (empty? l) (= n 0))
      null
      (cons (car l) (take (- n 1) (cdr l)) )
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
(define (is-palindrome? l)
  (equal? l (reverse l))
  )


; Задача 3: Да се дефинира функция (flatten ls), която приема списък от списъци ls и връща списък, съставен от елементите на списъците в ls
(define (flatten ls)
  (if (empty? ls)
      null
      (append (car ls) (flatten (cdr ls)))
      )
  )


; Задача 4: Да се дефинира функция (dot-product v1 v2), която връща като резултат скаларното произведение на векторите v1 и v2 (представени като списъци)
(define (dot-product v1 v2)
  (if (not (equal? (length v1) (length v2)))
      #f
      (if (empty? v1)
      0
      (+ (* (car v1) (car v2)) (dot-product (cdr v1) (cdr v2)))
      )
      )
  )


; Задача 5: Да се дефинира функция (sum-matrix m1 m2), която връща като резултат матрицата равна на
; сумата на матриците m1 и m2 (представени като списъци от списъци)
; Пример: (sum-matrix '((1 2) (3 4)) '((5 6) (7 8)) ) => '((6 8) (10 12))
(define (sum-vectors v1 v2)
  (if (not (equal? (length v1) (length v2)))
      #f
      (if (empty? v1)
          null
          (cons (+ (car v1) (car v2)) (sum-vectors (cdr v1) (cdr v2)))
          )
      )
  )
(define (sum-matrix m1 m2)
  (if (empty? m1)
      null
      (cons (sum-vectors (car m1) (car m2)) (sum-matrix (cdr m1) (cdr m2)))
      )
  )


; Задача 5*: Да се дефинира функция (matrix-mult m1 m2), която връща като резултат произведението на матриците m1 и m2
; Пример: (matrix-mult '((1 2 3) (4 5 6)) '((7 8) (9 10) (11 12)) ) => '((58 64) (139 154))
(define (dot-product_2 v1 v2)
  (if (empty? v1)
      0
      (+ (* (car v1) (car v2)) (dot-product_2 (cdr v1) (cdr v2)))
      )
  )
(define (matrix-mult m1 m2)
  (if (empty? m2)
      (if (empty? m1)
          null
          (cons (dot-product_2 (cdr m1) (map car m2)) (matrix-mult (cdr m1) (map cdr m2)))
          )
      (cons (dot-product_2 (car m1) (map car m2)) (matrix-mult (car m1) (map cdr m2)))
      )
  )


; Задача 6: Да се дефинира функция (transpose-matrix m), която връща транспсонираната матрица на m
; Пример: '((1 2) (3 4)) => '((1 3) (2 4))
(define (transpose-matrix m)
  (if (empty? (car m))
      null
      (cons (map car m) (transpose-matrix (map cdr m)) )
      )
  )


; Б) (nset-filter s pred?), която получава множество s и предикат pred?. Функцията връща ново множество от онези елементи на s, за които pred? връща #t.
(define (nset-filter s pred?)
  (define (nset-filter-helper s pred? countOfRemovedDigits newSet)
    (cond
      ((= s 0) newSet)
      ((or (= (remainder s 2) 0) (not (pred? countOfRemovedDigits)))
       (nset-filter-helper (quotient s 2) pred? (+ countOfRemovedDigits 1) newSet) )
      ((pred? countOfRemovedDigits)
       (nset-filter-helper (quotient s 2) pred? (+ countOfRemovedDigits 1) (+ newSet (expt 2 countOfRemovedDigits))) )
      )
    )
  (nset-filter-helper s pred? 0 0)
  )


; В) Напишете функция (nset-intersect s1 s2), която получава две множества s1 и s2 и връща ново множество -- тяхното сечение.
(define (contains? s2 elem)
  (if (= (nset-filter s2 (lambda (x) (= x elem))) 0)
      #f
      #t
      )
  )
(define (nset-intersect s1 s2)
  (nset-filter s1 (lambda (x) (contains? s2 x)))
  )


; Задача 1: Да се дефинира функция (nodes edges),
; която приема списък с ребрата edges на даден ориентиран граф
; (в който всяко ребро е представено като двойка (from . to))
; и връща списък, съдържащ всички върхове на съответния граф.
; '((1 . 2) (2 . 3) (1 . 4) (5 . 6) (2 . 4) (3 . 5))
(define (push-unique elem list)
  (if (member elem list)
      list
      (cons elem list)
      )
  )
(define (nodes edges)
  (foldl (lambda (edge l) (push-unique (cdr edge) (push-unique (car edge) l))  ) null edges )
  )


; Задача 2: Да се дефинира функция (adjacency-list edges),
; която приема списък с ребрата edges на даден ориентиран граф
; (в който всяко ребро е представено като двойка (from . to))
; и връща списъка на съседите на всички върхове.
; (т.е. върховете, към които има ребро от даден връх)
; '((1 . 2) (2 . 3) (1 . 4) (5 . 6) (2 . 4) (3 . 5))
; ->
;'( (
;  (1 . '(2 4))
;  (2 . '(3 4))
;  (3 . '(5))
;  (4 . '())
;  (5 . '(6))
;  (6 . '())
; )
(define (adjacency-list edges)
  (define (adjacency-list-helper vertex)
    (cons vertex (list (map cdr (filter (lambda (edge) (= (car edge) vertex)) edges))))
    )
  (map adjacency-list-helper (nodes edges))
  )
; (reverse (adjacency-list '((1 . 2) (2 . 3) (1 . 4) (5 . 6) (2 . 4) (3 . 5))))


; Задача 3: Да се дефинира функция (path? edges nodes),
; която приема списък с ребрата edges на даден
; ориентиран граф и списък от върхове nodes и връща
; дали списъкът nodes е път в графа описан от edges.
; > (path? '((1 . 2) (2 . 3) (1 . 4) (5 . 6) (2 . 4) (3 . 5)) '(1 2 3 4 5 6))
; #f
; > (path? '((1 . 2) (2 . 3) (1 . 4) (5 . 6) (2 . 4) (3 . 5)) '(2 3 6))
; #f
; > (path? '((1 . 2) (2 . 3) (1 . 4) (5 . 6) (2 . 4) (3 . 5)) '(1 2 3))
; #t
; > (path? '((1 . 2) (2 . 3) (1 . 4) (5 . 6) (2 . 4) (3 . 5)) '(1 2 4))
; #t
; > (path? '((1 . 2) (2 . 3) (1 . 4) (5 . 6) (2 . 4) (3 . 5)) '(2 3 5 6))
; #t

(define (contains-edge? edges edge)
  (not (equal? (filter (lambda (x) (and (= (car x) (car edge)) (= (cdr x) (cdr edge)))) edges) '()))
  )

(define (list-to-listOfPairs l)
  (if (= (length l) 1)
        null
        (cons (cons (car l) (cadr l)) (list-to-listOfPairs (cdr l)))
        )
  )

(define (path? edges nodes)
  (if (empty? (cdr nodes))
      #t
      (and (contains-edge? edges (car (list-to-listOfPairs nodes))) (path? edges (cdr nodes)))
      )
  )