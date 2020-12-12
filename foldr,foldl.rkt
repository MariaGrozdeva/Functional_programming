#lang racket

; Задача 0: Да се дефинира функция (sum-sq l), която пресмята
; сумата на квадратите на елементите на дадения списък l
(define (sum-sq l)
  (foldr (lambda (x acc) (+ (* x x) acc)) 0 l) ; acc- текущ резултат
  )                                            ; акумулирана стойност


; Задача 1: Да се дефинира функция (reverse l), която по даден списък l
; връща обърнатия списък на l
(define (reverse l)
  (foldl cons '() l)
  )


; Задача 2: Да се дефинира функция (increasing? l), която проверява
; дали елементите на списъка l образуват растяща редица
(define (increasing? l)
  (if (or (empty? l) (empty? (cdr l)) )
      #t
      (foldr (lambda (x isIncr) (if (< x (cadr l))
                                (increasing? (cdr l))
                                #f) )
         #t
         l
         )
      )
  )


; Задача 4: Да се дефинира фуннкция (unique l), която по подаден
; списък l връща само уникалните елементи от него
(define (unique l)
  (foldr (lambda (x uniqueList) (if (member x uniqueList)
                                    uniqueList
                                    (cons x uniqueList)))
           '()
           l
           )
  )


; Задача 5: Да се дефинира функция (intersect l1 l2), която връща сечението
; на списъците l1 и l2 (т.е. списък съдържащ общите им елементи)
(define (intersect l1 l2)
  (define (intersect-helper el list)
    (member el list) )
  (foldr (lambda (fleL intersection) (if (intersect-helper fleL l2)
                                         (cons fleL intersection)
                                         intersection
                                         ))
         '()
         l1
         )
  )


; Задача 6: Да се дефинира функция (sublists l), която по даден списък l
; връща всички подсписъци на дадения списък
(define (sublists l)
  (define (gen-lists x l)
    (append l (map (lambda (y) (cons x y)) l))
  )
  (foldr gen-lists '(()) l)
) ; .|.