#lang racket

(define (is-operator op) (or (char=? op #\+) (char=? op #\*) (char=? op #\/) (char=? op #\-) (char=? op #\^)))


(define (expr-valid-Help expr LastWasOp terminate ind)
(cond
  ((= ind (string-length  expr)) (not LastWasOp))
  ((char-whitespace?  (string-ref expr ind)) (expr-valid-Help expr LastWasOp #t (+ ind 1)))
  ((and LastWasOp  (char-numeric? (string-ref expr ind))) (expr-valid-Help expr #f #f (+ ind 1)))
  ((and (not LastWasOp) (not terminate)  (char-numeric? (string-ref expr ind))) (expr-valid-Help expr #f #f (+ ind 1)))
  ((and (not LastWasOp) (is-operator   (string-ref expr ind))) (expr-valid-Help expr #t #f (+ ind 1)))
  (else #f)))

(define (expr-valid? expr) (expr-valid-Help expr #t #f 0))


(expr-valid? "10  + 20")
(expr-valid? "10 20 + 5")
(expr-valid? "+++ 5 ")
(expr-valid? "+++")


(define (getLastHelp str num mult ind)
(if (or (= -1 ind) (char-whitespace? (string-ref str ind)))
    num
    (getLastHelp str (+ num (* mult(- (char->integer (string-ref str ind)) 48))) (* mult 10) (- ind 1)))
    )

(define (get-last str ind) (getLastHelp str 0 1 ind))
(define (get-last-last str ind)
  (if (char-whitespace? (string-ref str ind))
   (get-last str ( - ind 1))
   (get-last-last str (- ind 1))))


(define (getRes str ind)
(cond
  ( (char=?  (string-ref str ind) #\+) (number->string (+ (get-last str (- ind 2)) (get-last-last str (- ind 2)))) )
  ( (char=?  (string-ref str ind) #\-) (number->string (- (get-last str (- ind 2)) (get-last-last str (- ind 2)))) )
  ( (char=?  (string-ref str ind) #\*) (number->string (* (get-last str (- ind 2)) (get-last-last str (- ind 2)))) )
  ( (char=?  (string-ref str ind) #\/) (number->string (/ (get-last str (- ind 2)) (get-last-last str (- ind 2)))) )
  (else #f)))

(define (firstNumStart str ind count)
(if (or (= ind -1) (and (= count 2) (char-whitespace? (string-ref str ind))))
    (+ ind 1)
    (if (char-whitespace? (string-ref str ind))
        (firstNumStart str (- ind 1) (+ count 1))
        (firstNumStart str (- ind 1) count))))        

(define (changeString str ind) (string-replace str  (substring str (firstNumStart str (- ind 1) 0) (+ ind 1)) (getRes str ind)))  
  
(define (RPNtoNumHelp str ind)
(cond
  ((= ind (string-length  str)) str)
  ((is-operator (string-ref str ind)) (RPNtoNumHelp (changeString str ind) 0))
  (else (RPNtoNumHelp str (+ 1 ind)))))

(define (expr-eval str) (RPNtoNumHelp str 0))

(expr-eval "3 10 5 + *")