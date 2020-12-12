#lang racket/base

; Да се напише функция (max-unique ll), която по списък от списъци от цели числа намира най-голямото от тези от тях, които са
; уникални в рамките на списъка, в който се срещат. Ако в никой списък няма уникални числа, функцията да връща #f.
; (max-unique '((1 2 3 2) (5 5) (0))) --> връща 3
; (max-unique '((1 2 1 2) (5 5) ()))  --> връща #f
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



; Да се напише функция (longest-descending­ l), която намира низходящо сортиран подсписък
; на списъка от числа l с максимална дължина.
; Ако съществуват няколко такива подсписъка, функцията да върне първия отляво надясно.
; Упътване: Реализирайте помощна функция, която намира най-дългия низходящо сортиран префикс на даден списък.
; (longest-descending '(5 3 8 6 4 2 6 7 1))     --> връща '(8 6 4 2)
; (longest-descending '(1 2 3 4 5 6))           --> връща '(1)
; (longest-descending '(5 3 8 6 4 2 6 5 4 1))   --> връща '(8 6 4 2)
; (longest-descending '(5 3 8 6 4 2 6 5 4 1 0)) --> връща '(6 5 4 1 0)
(define (longest-descending-prefix l)
  (define (longest-descending-prefix-helper l lastDigit)
    (if (or (null? l) (< lastDigit (car l)))
        '()
        (cons (car l) (longest-descending-prefix-helper (cdr l) (car l))) )
    )
  (longest-descending-prefix-helper l +inf.0)
  )

(define (descending-sublists l)
  (if (null? l)
      '()
      (cons (longest-descending-prefix l) (descending-sublists (cdr l))) )
  )

(define (longest-descending l)
  (define (longest-descending-helper l)
    (foldr (lambda (x y) (if (>= (length x) (length y)) x y)) '() l)
    )
  (define desc-subl (descending-sublists l))
  (longest-descending-helper desc-subl)
  )



; Казваме, че списъкът x = (x1 x2 ... x2n) от цели числа се получава от прочитането (look-and-say) на списъка y, ако y се състои от
; последователно срещане на x1 пъти x2, последвано от x3 пъти x4, и така нататък до x2n-1 пъти x2n. Да се дефинира функция next-
; look-and-say, която по даден списък y намира списъка x, получен от прочитането y.
; (look-and-say '(1 1 2 3 3)) --> връща ‘(2 1 1 2 2 3)
(define (group-consecutive l)
  (foldr
   (lambda (x y)
     (if (or (null? y) (not (= (car (car y)) x)))
         (cons (list x) y)
         (cons (cons x (car y)) (cdr y)) ) )
     '()
     l )
  )
; --> '((1 1) (2) (3 3))

(define (look-and-say l)
  (define ll (group-consecutive l))
  (foldr append '() (foldr
                     (lambda (x y) (cons (list (length x) (car x)) y)) '() ll) )
  )
; --> '(2 1 1 2 2 3)



; Нека е даден списък l от числа и двуместна операция над числа ⊕.
; Функцията f наричаме “ендоморфизъм над l”, ако f трансформира l в себе си, запазвайки операцията ⊕, т.е.
; ∀x∈l f(x)∈l и
; ∀x,y∈l f(x) ⊕ f(y) = f(x ⊕ y).
; Да се реализира функция is-em?, която проверява дали f е ендоморфизъм.
; (is-em? '(0 1 4 6) + (lambda (x) (remainder x 3))) --> връща #t
(define (map-into-itself? l f)
  (define (map-into-itself-helper new)
    (if (null? new)
        #t
        (if (member (car new) l)
            (map-into-itself-helper (cdr new))
            #f ) )
    )
  (define newL (map f l))
  (map-into-itself-helper newL)
  )
; (map-into-itself? '(0 1 4 6) (lambda (x) (remainder x 5))) --> #t
; (map-into-itself? '(0 1 4 6) (lambda (x) (remainder x 4))) --> #f

(define (ordered-pairs l)
  (define (ordered-pairs-helper l1 l2)
    (if (null? l1)
        '()
        (if (null? l2)
            (ordered-pairs-helper (cdr l1) l)
            (cons (cons (car l1) (car l2)) (ordered-pairs-helper l1 (cdr l2))) ) )
    )
  (ordered-pairs-helper l l)
  )
; (ordered-pairs '(1 3)) --> '((1 . 1) (1 . 3) (3 . 1) (3 . 3))

(define (closed-under-operation l op f)
  (foldr
   (lambda (x y)
     (if (= (f (op (car x) (cdr x))) (op (f (car x)) (f (cdr x))))
         y
         #f ) )
     #t
     (ordered-pairs l) )
  )
; (closed-under-operation '(0 1 4 6) + (lambda (x) (remainder x 3))) --> #t
; (closed-under-operation '(0 1 4 6) + (lambda (x) (remainder x 5))) --> #f

(define (is-em? l op f)
  (and (map-into-itself? l f) (closed-under-operation l op f) )
  )



; BINARY TREES: '(10 (5 () ()) (13 (12 (11 () ()) ()) (20 () ())))

; 1.Дефинирайте функция (sum tree), която намира сумата на всички елементи на дървото tree.
(define (sum tree)
  (if (null? tree)
      0
      (+ (car tree) (sum (cadr tree)) (sum (caddr tree))) )
  )


; 2.Дефинирайте функция (map-tree function tree).
(define (map-tree function tree)
  (if (null? tree)
      '()
      (list (function (car tree)) (map-tree function (cadr tree)) (map-tree function (caddr tree))) )
  )


; 3.Дефинирайте функция (level tree i), която връща списък от стойностите на възлите, намиращи се на дълбочина i от корена.
(define (level tree i)
  (if (not (null? tree))
      (if (= i 1)
          (list (car tree))
          (append (level (cadr tree) (- i 1)) (level (caddr tree) (- i 1))) )
      '()
      )
  )



; 4.Дефинирайте функции (inorder tree), (preorder tree) и (postorder tree), които връщат списък от всички елементи на tree, получени при съответното обхождане.
(define (inorder tree)
  (if (null? tree)
      '()
      (append (inorder (cadr tree)) (list (car tree)) (inorder (caddr tree))) )
  )
(define (preorder tree)
  (if (null? tree)
      '()
      (append (list (car tree)) (preorder (cadr tree)) (preorder (caddr tree))) )
  )
(define (postorder tree)
  (if (null? tree)
      '()
      (append (postorder (cadr tree)) (postorder (caddr tree)) (list (car tree)) ) )
  )



; 5.Дефинирайте функция (binary-search-tree? tree), която проверява дали дървото tree е наредено.
(define (binary-search-tree? tree)
  (define (binary-search-tree-helper tree min max)
    (if (null? tree)
        #t
        (if (or (> (car tree) max) (< (car tree) min))
            #f
            (and (binary-search-tree-helper (cadr tree) min (- (car tree) 1)) (binary-search-tree-helper (caddr tree) (+ (car tree) 1) max)) ) )
        )
    (binary-search-tree-helper tree -inf.0 +inf.0)
    )



; 6.1.Дефинирайте функция (construct-binary-search-tree sorted-list), която по даден сортиран списък конструира двоично наредено дърво.
(define (get-middle-of-list l)
  (if (= (length l) 1)
      1
      (if (= (remainder (length l) 2) 1)
          (+ (quotient (length l) 2) 1)
          (quotient (length l) 2) ) )
  )
(define (element-at-index l ind)
  (if (= ind 1)
      (car l)
      (element-at-index (cdr l) (- ind 1)) )
  )
(define (get-n-elements list n)
  (if (= n 1)
      '()
      (cons (car list) (get-n-elements (cdr list) (- n 1))) )
  )
(define (drop-n-elements list n)
  (if (= n 1)
      (cdr list)
      (drop-n-elements (cdr list) (- n 1)) )
  )

(define (construct-binary-search-tree sorted-list)
  (if (= (length sorted-list) 0)
      '()
      (list (element-at-index sorted-list (get-middle-of-list sorted-list))
            (construct-binary-search-tree (get-n-elements sorted-list (get-middle-of-list sorted-list)))
            (construct-binary-search-tree (drop-n-elements sorted-list (get-middle-of-list sorted-list))) ) )
  )


; 6.1.Дефинирайте помощна функция (insert binary-search-tree element),която вмъква element на подходящото място в двоично наредено дърво.
(define (insert binary-search-tree element)
  (if (null? binary-search-tree)
      (list element null null)
      (if (< element (car binary-search-tree))
          (list (car binary-search-tree) (insert (cadr binary-search-tree) element) (caddr binary-search-tree))
          (list (car binary-search-tree) (cadr binary-search-tree) (insert (caddr binary-search-tree) element)) ) )
  )



; 7.Дефинирайте фунцкия (contains? tree path), която проверява дали даден път path - списък от стойности, се съдържа в tree.
; (contains? example-tree '(10 13 12)) --> връща #t
; (contains? example-tree '(10 5 12)) --> връща #f
(define (contains? tree path)
  (if (null? path)
      #t
      (if (null? tree)
          #f
          (if (= (car path) (car tree))
              (or (contains? (cadr tree) (cdr path)) (contains? (caddr tree) (cdr path)))
              (or (contains? (cadr tree) path) (contains? (caddr tree) path)) ) ) )
  )



; 8.Дефинирайте функция (symmetric? tree), която проверява дали дадено дърво е симетрично (разглеждаме само структурата му, не и стойностите във възлите му).
; (symmetric? '(1 (2 () (4 () ())) (3 (5 () ()) ()))) --> връща #t
(define (reverse tree)
  (if (null? tree)
      null
      (list (car tree) (reverse (caddr tree)) (reverse (cadr tree))) )
  )

(define (symmetric? tree)
  (define (symmetric-helper tree-left tree-right)
    (if (and (null? tree-left) (null? tree-right))
        #t
        (if (or (and (null? tree-left) (not (null? tree-right))) (and (null? tree-right) (not (null? tree-left))))
            #f
            (and (car tree-left) (car tree-right)
                 (symmetric-helper (cadr tree-left) (cadr tree-right)) (symmetric-helper (caddr tree-left) (caddr tree-right))) ) )
    )
  (symmetric-helper (cadr tree) (reverse (caddr tree)))
  )



; 9.Дефинирайте функция (paths tree), която намира всички пътища в tree.
(define (paths tree)
  (if (null? tree)
      '()
      (if (and (null? (cadr tree)) (null? (caddr tree)))
          (list (list (car tree)))
          (map (lambda (x) (cons (car tree) x))
               (append (paths (cadr tree)) (paths (caddr tree)))) ) )
  )
; (paths '(10 (5 () ()) (1 (2 () ()) ()))) --> връща '((10 5) (10 1 2))