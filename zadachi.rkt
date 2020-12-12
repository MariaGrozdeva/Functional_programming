#lang racket

;1
(define (Power n k)
  (if(= k 0)
     1
     (* n (Power n (- k 1))) )
)

;2
(define (inc-digits n)
  (if( < n 10)
     #t
     (and (> (remainder n 10) (remainder (quotient n 10) 10))  (inc-digits (quotient n 10))))
  )

;3
(define (CountOfDigits n)
  (if( < n 10)
     1
     (+ 1 (CountOfDigits (quotient n 10))))
  )

;4
(define (Reverse n)
  (if( < n 10)
     n
     (+ (* (remainder n 10) (Power 10 (- (CountOfDigits n) 1))) (Reverse (quotient n 10))))
  )

;5
(define (IsPalindrome n)
     (= n (Reverse n))
)

;6
(define (bcd x y)
  (if (= x 0)
     y
     (if (< y x)
         (bcd y x)
         (bcd (remainder y x) x)))
  )

;7
(define (GetLastDigit n)
  (remainder n 10)
  )

;8
(define (CountOfKs n k)
  (if( < n 10)
     (if( = n k)
        1
        0)
     (if( = (GetLastDigit n) k)
        ( + (CountOfKs (quotient n 10) k)  1)
        (CountOfKs (quotient n 10) k)))
  )