#lang racket
; delay, force

(define (head stream) (car stream))
(define (tail stream) (force (cdr stream)))
; L :- '()
; L :- (x . L1)
; потоци
; S :- '()
; S :- (x . <promise S>)

; Задача 0: Дефинирайте функция (repeat i), която създава безкраен поток от елемента i
; списъци
(define (repeat i)
  (cons i (delay (repeat i)))
  )
; > (repeat 1)
; '(1 . #<promise:unsaved-editor:15:10>)
; > (define stream-one (repeat 1))
; > (head stream-one)
; 1
; > (tail stream-one)
; '(1 . #<promise:unsaved-editor:15:10>)
; > (head (tail stream-one))
; 1


; Задача 1: Дефинирайте функция (stream-take s n), която взима първите n елемента на потока s
; Ако s е с дължина по-малка от n, да се върнат колкото елементи има
(define stream-one (repeat 1))
(define (stream-take s n)
  (if (or (= n 0) (empty? s))
      null
      (cons (head s) (stream-take (tail s) (- n 1)))
      )
  )


; Задача 2: Дефинирайте функция (stream-drop s n), която връща поток без първите n елемента на s
; Ако s е с дължина по-малка от n, да се върне празен списък
(define (stream-drop s n)
  (cond ((empty? s) null)
        ((= n 0) s)
        (else (stream-drop (tail s) (- n 1))) )
  )


; Задача 3: Дефинирайте символ naturals-stream, който да съдържа
; безкраен поток от последователни естествени числа
(define (naturals n)
  (cons n (delay (naturals (+ n 1))))
  )
(define naturals-stream
  (naturals 1)
  )


; Задача 4: Дефинирайте функция (generate init f), която генерира
; безкрайния поток init, f(init), f(f(init) ...
(define (generate init f)
  (cons init (delay (generate (f init) f)))
  )


; Задача 5: Дефинирайте функция (stream-map s f), която генерира поток от
; резултатите на функцията f приложена върху елементите на s
(define (stream-map s f)
  (if (empty? s)
      null
      (cons (f (head s)) (delay (stream-map (tail s) f)) )
      )
)


; Задача 6: Дефинирайте символ squares, който да съдържа
; безкраен поток от квадратите на всички естествени числа
(define squares (stream-map naturals-stream (lambda (x) (* x x))) )


; Задача 7: Дефинирайте функция (stream-filter s p), която генерира
; поток от елементите на s, за които (p x) връща истина
(define (stream-filter s p)
  (if (empty? s)
      null
      (if (p (head s))
          (cons (head s) (delay (stream-filter (tail s) p)))
          (stream-filter (tail s) p)
          )
      )
  )
; (stream-take (stream-filter naturals-stream odd?) 20)


; Задача 8: Дефинирайте символ отговарящ на потока от естествени числа,
; които отговарят на условието (x^2 + 1) % 3 == 0
(define filtered-naturals (stream-filter naturals-stream (lambda (x) (= (remainder (- (* x x) 1) 3) 0) ) ) )