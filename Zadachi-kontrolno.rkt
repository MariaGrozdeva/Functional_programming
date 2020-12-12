; Задача 1: Да се дефинира функция (revlex-less a b), която проверява дали a е преди b в лексикографска наредба (откъм последния символ)
(define (revlex-less a b)
  (if (and (= a 0) (= b 0))
      #t
      (if (< (remainder a 10) (remainder b 10))
          #t
          (if (> (remainder a 10) (remainder b 10))
              #f
              (revlex-less ((quotient (a 10)) (quotient (b 10))) )
              )
          )
      )
  )

; Задача 2: Да се дефинира функция (nset-accumulate op term init s), която работи сходно на функцията accumulate, но прилага (op) върху елементите на множеството s
(define (nset-accumulate op term init s)
  (define (nset-acc-helper op term init s n)
    (if (= s 0)
        init
        (if (= (remainder s 2) 1)
            (nset-acc-helper op term (op init (term n)) (quotient s 2) (+ n 1))
            (nset-acc-helper op term init (quotient s 2) (+ n 1))
            )
        )
    )
  (nset-acc-helper op term init s 0)
  )

; Задача 3: Да се дефинира функция (nset-revlex-min s), която намира най-малкия (според наредбата дефинирана в Задача 1) елемент на множеството s. Ако множеството е празно, функцията да връща #f.
(define (nset-revlex-min s)
  (define (op acc x)
    (if (not acc)
        x
        (if (revlex-less acc x)
            acc
            x
            )
        )
    )
  (nset-accumulate op id #f s)
  )

; Задача 4: В даден ресторант постоянно получават заявки за резервации за различни времена от деня и с различна продължителност. За да организират оптимален работен график, трябва да имат информация за най-натоварените части на деня. Да се дефинира функция (find-busiest reservations), която по списък с резервации намира най-натоварения интервал и колко резервации има за него. Резервациите са наредени двойки от естествени числа, сигнализиращи начало и край на резервацията. Резервации с начало не по-малко от края считаме за невалидни.
(define (sort ls)
  (if (null? ls)
      ls
      (
       append
      (sort (filter (lambda(x) (< x (car ls))) (cdr ls)))
      (list (car ls))
      (sort (filter (lambda(x) (>= x (car ls))) (cdr ls)))
      )
  )
  )

;'(1 2 4 5 6) '(10 5 7 7 7)
;((1.10), (2.5), (4.7), (5.7), (6.7))
;starts: '(1 2 4 5 6)
;ends: '(5 7 7 7 10)
(define (find-busiest reservations)
  (define valid-reservations (filter (lambda (res) (< (car res) (cdr res))) reservations))
  (define starts (sort (map car valid-reservations)))
  (define ends   (sort (map cdr valid-reservations)))
  (define (reservations-helper starts-left ends-left current-busiest current-accumulating)
    (if (null? starts-left)
        current-busiest
        (if (< (car starts-left) (car ends-left))
            (reservations-helper (cdr starts-left)
                                 ends-left
                                 current-busiest
                                 (cons
                                  (cons (car starts-left) (cdar current-accumulating))
                                  (cdr current-accumulating)
                                  )
                                 )
            )
        )
    )
  )