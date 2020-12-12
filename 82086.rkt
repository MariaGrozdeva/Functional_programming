;
; СУ "Св. Климент Охридски"
; Факултет по математика и информатика
; Курс Функционално програмиране 2020/21
; Контролно 2
; 2020-12-12
;
; Име: Мария Гроздева
; ФН: 82 086
; Специалност: Компютърни науки
; Курс: 2
; Административна група: 7
; Начален час на контролното: <тук попълнете часа за вашата група> 11:30
;

#lang racket/base

(provide (all-defined-out))

; 1
; (5 (6 (7 () ()) (8 () ())) (9 () (11 (12 () ()) ())))
(define (min-heap-helper tree min)
  (if (null? tree)
      #t
      (if (>= (car tree) min)
          (and (min-heap-helper (cadr tree) (car tree)) (min-heap-helper (caddr tree) (car tree)))
          #f ) )
  )
(define (max-heap-helper tree max)
  (if (null? tree)
      #t
      (if (<= (car tree) max)
          (and (max-heap-helper (cadr tree) (car tree)) (max-heap-helper (caddr tree) (car tree)))
          #f ) )
  )

(define (heap? tree)
  (or (min-heap-helper tree -inf.0) (max-heap-helper tree +inf.0))
  )
; (heap? '(5 (6 (7 () ()) (8 () ())) (9 () (11 (12 () ()) ())))) #t
; (heap? '(10 (9 (8 () ()) (7 () ())) (6 () (5 (4 () ()) ())))) #t
; (heap? '(5 (4 (7 () ()) (8 () ())) (9 () (11 (12 () ()) ())))) #f
; (heap? '(10 (11 (8 () ()) (7 () ())) (6 () (5 (4 () ()) ())))) #f



; 2
; '((1 5) (3 6) (4 9) (2 3))
; sqrt( (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) )
;(define (append-into-one-list pts)
;  (foldr append '() pts) )
; --> '(1 5 3 6 4 9 2 3)

;(define (cost pts)
;  (if (or (= (length pts) 0) (= (length pts) 1))
;      0
;      (sqrt (+ (* (- (car (car pts)) (car (cadr pts))) (- (car (car pts)) (car (cadr pts))))
;               (* (- (cdr (car pts)) (cdr (cadr pts))) (- (cdr (car pts)) (cdr (cadr pts)))))) )
;  )