#lang racket/base
(provide (all-defined-out))

; (TREE? STR)
; removing whitespaces
(define (remove-whitespaces str)
  (if (= (string-length str) 0)
      ""
      (if (eq? (string-ref str 0) #\space)
          (remove-whitespaces (substring str 1))
          (string-append (string (string-ref str 0)) (remove-whitespaces (substring str 1) ) )
          )
      )
  )



; removing first number
(define (is-digit ch) (and (char>=? ch #\0) (char<=? ch #\9)) )
(define (remove-digits-start str)
  (if (string=? str "")
      ""
      (if (is-digit (string-ref str 0))
          (remove-digits-start (substring str 1))
          str)
  )
)
 


; checking syntax 
(define (is-valid-symbol ch) (or (is-digit ch) (eq? ch #\*) (eq? ch #\{) (eq? ch #\})) )
(define (check-syntax str)
  (define (check-syntax str bracketCount)
    (if (< bracketCount 0)
        #f
        (if (= (string-length str) 0)
            (if (= bracketCount 0)
                 #t
                 #f )
            (if (not (is-valid-symbol (string-ref str 0 )))
                #f
                (if (eq? (string-ref str 0) #\{)
                    (check-syntax (substring str 1) (+ bracketCount 1))
                    (if (eq? (string-ref str 0) #\})
                       (check-syntax (substring str 1) (- bracketCount 1))
                       (check-syntax (substring str 1) bracketCount) ) ) ) ) )
    )
  (check-syntax str 0)
  )



; finding the end index of a child
(define (find-mid str)
  (define (find-mid str bracketsCount ind)
    (if (string=? str "")
        #f
        (if (eq? (string-ref str 0) #\})
            (if (= bracketsCount 1)
                ind
                (find-mid (substring str 1) (- bracketsCount 1) (+ 1 ind)) )
            (if (eq? (string-ref str 0) #\{)
                (find-mid (substring str 1) (+ bracketsCount 1) (+ 1 ind))
                (if (eq? (string-ref str 0) #\*)
                    (if (= bracketsCount 0)
                        ind
                        (find-mid (substring str 1) bracketsCount (+ 1 ind)) )
                    (if (is-digit (string-ref str 0))
                        (find-mid (substring str 1) bracketsCount (+ 1 ind))
                        #f) ) ) ) )
    )
    (find-mid str 0 0)
  )



; removing first and last brackets and first number
(define (rem-dig-and-br str)
  (if (and (eq? (string-ref str 0) #\{) (eq? (string-ref str (- (string-length str) 1)) #\}))
      (if (string=? (substring str 1 (- (string-length str) 1)) (remove-digits-start (substring str 1 (- (string-length str) 1))))
          #f
          (remove-digits-start (substring str 1 (- (string-length str) 1))) )
      #f )
  )



; main function
(define (tree? str)
  (define (tree-rec? string)
    (if (= (string-length string) 0)
        #f
        (if (= (string-length string) 1)
        (if (eq? (string-ref string 0) #\*)
            #t
            #f)
        (if (eq? (rem-dig-and-br string) #f)
            #f
            (and (number? (find-mid (rem-dig-and-br string))) (number? (find-mid (rem-dig-and-br string))) (tree-rec? (substring (rem-dig-and-br string) 0 (+ (find-mid (rem-dig-and-br string)) 1)))
                 (tree-rec? (substring (rem-dig-and-br string) (+ (find-mid (rem-dig-and-br string)) 1)))
                 ))))
    )
  (define withoutSpaces (remove-whitespaces str) )
  (and (check-syntax withoutSpaces) (tree-rec? withoutSpaces))
  )





; (STRING->TREE STR)
(define (extract-number str)
  (define (extract-number-helper str number)
    (if (is-digit (string-ref str 0))
        (extract-number-helper (substring str 1) (+ (* number 10) (string->number (string (string-ref str 0)))) )
        number )
    )
  (extract-number-helper str 0)
  )



(define (string->tree str)
  (define (string->tree-helper str)
    (if (= (string-length str) 1)
          '()
          (append (list (extract-number (substring str 1)))
                  (list (string->tree-helper (substring (rem-dig-and-br str) 0 (+ (find-mid (rem-dig-and-br str)) 1))))
                  (list (string->tree-helper (substring (rem-dig-and-br str) (+ (find-mid (rem-dig-and-br str)) 1))))) 
    )
    )
 (define withoutSpaces (remove-whitespaces str) )
 (define newString (if (= (string-length withoutSpaces) 1)
                       withoutSpaces
                       (substring withoutSpaces 1 (- (string-length withoutSpaces) 1))) )
 (if (eq? (tree? withoutSpaces) #f)
     #f
     (string->tree-helper withoutSpaces) )
  )





; (BALANCED? TREE)
(define (height tree)
  (if (eq? tree '())
        -1
        (+ 1 (max (height (at tree 2)) (height (at tree 3)))) )
  )


(define (balanced?-helper tree)
  (<= (abs (- (height (at tree 2)) (height (at tree 3)))) 1)
  )


(define (balanced? tree)
  (if (eq? tree '())
      #t
      (and (balanced?-helper tree) (balanced? (at tree 2)) (balanced? (at tree 3))) )
  )





; (ORDERED? TREE)
(define (at l n)
    (cond ((null? l) l)
          ((= n 1) (car l))
          (else (at (cdr l) (- n 1))) )
  )



(define (get-max-number-in-child child)
  (define (get-max-number-in-child-helper child maxNum)
  (if (eq? child '())
      maxNum
      (if (number? (car child))
          (if (> (car child) maxNum)
              (get-max-number-in-child-helper (cdr child) (car child))
              (get-max-number-in-child-helper (cdr child) maxNum) )
          (if (and (not (eq? (car child) '())) (list? (car child)))
              (if (> (car (car child)) maxNum)
                  (get-max-number-in-child-helper (append (cdr (car child))) (car (car child)))
                  (get-max-number-in-child-helper (append (cdr (car child))) maxNum) )
              (if (and (eq? (car child) '()) (not (= (length child) 0)))
                  (get-max-number-in-child-helper (cdr child) maxNum)
                  maxNum ))))
  )
(get-max-number-in-child-helper child -inf.0)
)

(define (get-min-number-in-child child)
  (define (get-min-number-in-child-helper child minNum)
  (if (eq? child '())
      minNum
      (if (number? (car child))
          (if (< (car child) minNum)
              (get-min-number-in-child-helper (cdr child) (car child))
              (get-min-number-in-child-helper (cdr child) minNum) )
          (if (and (not (eq? (car child) '())) (list? (car child)))
              (if (< (car (car child)) minNum)
                  (get-min-number-in-child-helper (append (cdr (car child))) (car (car child)))
                  (get-min-number-in-child-helper (append (cdr (car child))) minNum) )
              (if (and (eq? (car child) '()) (not (= (length child) 0)))
                  (get-min-number-in-child-helper (cdr child) minNum)
                  minNum ))))
  )
(get-min-number-in-child-helper child +inf.0)
)



(define (ordered-helper vertex child1 child2)
  (if (and (> vertex (get-max-number-in-child child1)) (< vertex (get-min-number-in-child child2)))
      #t
      #f )
  )
   
(define (ordered? tree)
  (if (= (length tree) 0)
      #t
      (and (ordered-helper (car tree) (at tree 2) (at tree 3)) (ordered? (at tree 2)) (ordered? (at tree 3)) )
  )
  )





; (TREE->STRING TREE)
(define (tree->string tree)
  (if (eq? tree '())
        (string #\*)
        (if (number? tree)
            (number->string tree)
            (string-append "{" (tree->string (car tree)) " " (tree->string (at tree 2)) " " (tree->string (at tree 3)) "}") ) )
  )