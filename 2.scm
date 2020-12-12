#lang racket

;1
(define (set-add set elem)
  (bitwise-ior set (arithmetic-shift 1 elem))
  )

;2
(define (set-remove set elem)
  (if (set-contains? set elem)
       (bitwise-xor set (arithmetic-shift 1 elem))
       set
       )
  )

;3
(define (set-contains? set elem)
  (if (= (bitwise-and set (arithmetic-shift 1 elem)) 0)
      #f
      #t
      )
  )

;4
(define (set-empty? set)
  (if (= (bitwise-xor set 1) 1)
      #t
      #f
      )
  )

;5
(define (set-size set)
  (define (count-set-bits set count)
    (if (= set 0)
        count
        (count-set-bits (arithmetic-shift set -1) (+ count (bitwise-and set 1)) )
        )
    )
      (count-set-bits set 0)
  )

;6
(define (set-intersect s1 s2)
  (bitwise-and s1 s2)
  )

;7
(define (set-union s1 s2)
  (bitwise-ior s1 s2)
  )

;8
(define (set-difference s1 s2)
  (bitwise-and s1 (bitwise-not s2))
   )