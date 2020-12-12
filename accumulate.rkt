#lang racket

;HIGHER ORDER FUNCTIONS

;1.Integration
(define (integrate f a b step)
  (if (> a b)
      0
      (+ (* (f a) step) (integrate f (+ a step) b step)))
  )


;2.ACCUMULATE
(define (1+ n) (+ 1 n))

;2.0
(define (prn result i) (display i))
(define (accumulate-i init op a next b)
  (define (loop result i)
    (if (<= i b) 
        (loop (op result i) (next  i))
        result
        )
    )
  (loop init a)
  )

;2.1. Iterative version; op-: (( ... (((0 – 10) – 11) – 12) ... 19) - 20)
(define (accumulate-r init op a next b)
  (define (loop i)
    (if(<= i b)
      (op i (loop(next i)) )
      init)
    )
  (loop a)
  )

;2.2. Recursive version; op-: (( ... ((0 - 20) - 19) – 18 ... ) - 10)
(define (accumulate-rec init op a next b)
   (define (loop i)
     (if (<= i b)
         (op (loop (next i)) i)
         init)
     )
   (loop a)
  )

; ACCUMULATE
(define (accumulate op term init a next b)
  (define (loop i)
    (if (<= i b)
        (op (term i) (loop (next i)) )
        init)
    )
  (loop a)
  )
; ACCUMULATE

;2.3. Sum of nums in an interval
(define (id x) x)
(define (sum-int a b)
  (accumulate + id 0 a 1+ b)
  )

;2.4. Power series
(define (sum-row base a b)
  (define (term t) (expt base t))
  (accumulate + term 0 a 1+ b)
  )

;2.5. Iterative function accumulate
(define (accumulate-it op term init a next b)
  (define (loop result i)
    (if (> i b)
        result
        (loop (op result (term i)) (next i)) )
    )
  (loop init a)        
  )

; Sum of even nums in an interval
(define (2+ n) (+ n 2))
(define (sum-even a b)
  (accumulate-it +
                 id
                 0
                 (if (= (remainder a 2) 0) a (+ a 1) )
                 2+
                 b)
  )

; FILTER-ACCUMULATE
(define (filter-accumulate p? op term init a next b)
  (define (loop i)
    (cond ((> i b) init)
          ((p? i) (op (term i) (loop (next i))))
          (else (loop (next i))) )
    )
  (loop a)
  )
; FILTER-ACCUMULATE



;LAMBDA
; Composition
(define (Square x) (* x x))
(define (compose f g)
  (lambda (x)
    (f (g x)) )
  )
(define double-square (compose Square Square))

; Fold
(define (repeated f N)
  (cond ((= N 0) (lambda(x) x))
        ((= N 1) f)
        (else (compose f
                       (repeated f (- N 1))) 
              )
        )
  )
   (define sq2 (repeated Square 2))                    



; Задача 0: Да се дефинира функция (sum-divisors n a b), която връща като резултат сумата на числата в затворения интервал [a, b], които са делители на n
;0.0.
(define (Sum-divisors n a b)
  (define (loop result i)
    (cond ((> i b) result)
          ((not (= (remainder n i) 0)) (loop result (+ 1 i)))
          ((= (remainder n i) 0) (loop (+ result i) (+ 1 i)))
          )
    )
  (loop 0 a)
  )
;0.1.
(define (sd n a b)
  (filter-accumulate (lambda (x) (= (remainder n x) 0) )
                     +
                     id
                     0
                     a
                     1+
                     b )
  )

; Задача 0*: Да се дефинира функция (sum-squares a b), която връща като резултат сумата от квадратите на числата в затворения интервал [a, b]
(define (sum-squares a b)
  (accumulate +
              (lambda (x) (* x x) )
               0
               a
               1+
               b )
  )
                          
; Задача 1: Да се дефинира функция (sum-mapped f a b), която връща като резултат сумата на резултата на изпълнението на функцията f върху числата в затворения интервал [a, b]
(define (sum-mapped f a b)
 (if (> a b)
     0
     (+ (f a)(sum-mapped f (+ a 1) b)) )
  )

; Задача 2: Да се дефинира функция, която връща като резултат произведението на всички четни числа в интервала [a, b]
(define (product-mapped a b)
 (accumulate
           *
           id
           1
           (if (= (remainder a 2) 0) a
               (+ 1 a) )
           2+
           b)
  )

; Задача 3: Да се дефинира функция, която връща като резултат броя на всички точни квадрати в интервала [a, b]
(define (num-squares a b)
  (filter-accumulate
   (lambda (x) (= (floor (sqrt x)) (sqrt x)))
           +
           (lambda (x) 1)
           0
           a
           1+
           b)
  )

; Задача 4: Да се дефинира функция, която проверява дали съществува число в интервала [a, b], за което функцията f да връща #t
(define (func-to-check f a b)
  (if (> a b)
      #f
  (or (f a) (func-to-check f (+ 1 a) b)) )
  )

; Задача 5: Да се дефинира функция (compose f g), която връща като резултат композицията на функциите f и g
(define (Compose f g)
  (lambda (x)
    (f (g x))
    )
  )

; Задача 6: Да се дефинира функция (repeat f n), която връща като резултат функция, еквивалентна на прилагането на функцията f n-пъти
(define (repeat f n)
  (cond ((= n 0) (lambda (x) x))
        ((= n 1) f)
        (else (compose f
                       (repeat f (- n 1))) )
        )
  )
(define square-n (repeat Square 4))