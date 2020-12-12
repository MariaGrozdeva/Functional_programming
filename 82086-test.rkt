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

(require "82086.rkt") 

(require rackunit rackunit/gui)

(define (expect-min-heap tree)
  (test-true
   "Tree should be min-heap"
   (min-heap-helper tree -inf.0)
   )
  )
(define (expect-not-min-heap tree)
  (test-false
   "Tree should not be min-heap"
   (min-heap-helper tree -inf.0)
   )
  )

(define (expect-max-heap tree)
  (test-true
   "Tree should be max-heap"
   (max-heap-helper tree +inf.0)
   )
  )
(define (expect-not-max-heap tree)
  (test-false
   "Tree should not be max-heap"
   (max-heap-helper tree +inf.0)
   )
  )

(define (expect-heap tree)
  (test-true
   "Tree should be heap"
   (heap? tree)
   )
  )
(define (expect-not-heap tree)
  (test-false
   "Tree should not be heap"
   (heap? tree)
   )
  )
   

(test/gui
 
 (test-suite
  "min-heap-helper works correctly"
  (expect-min-heap '(5 (6 (7 () ()) (8 () ())) (9 () (11 (12 () ()) ())))) 
  (expect-not-min-heap '(5 (4 (7 () ()) (8 () ())) (9 () (11 (12 () ()) ()))))
  )
  
 (test-suite
  "max-heap-helper works correctly"
  (expect-max-heap '(10 (9 (8 () ()) (7 () ())) (6 () (5 (4 () ()) ()))))
  (expect-not-max-heap '(10 (11 (8 () ()) (7 () ())) (6 () (5 (4 () ()) ()))))
  )

 (test-suite
  "heap? works correctly"
  (expect-heap '(5 (6 (7 () ()) (8 () ())) (9 () (11 (12 () ()) ()))))
  (expect-not-heap '(10 (11 (8 () ()) (7 () ())) (6 () (5 (4 () ()) ()))))
  )
 
 )