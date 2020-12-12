(define (draw-sides-left n)
  (define (draw-sides-left-help n)
    (if (> n 1)
        (draw-sides-left-help (- n 1)) )
    (display "\u2502")
    (display " ")
    )
  
  (if (> n 0)
     (draw-sides-left-help n) )
  )

(define (draw-sides-right n)
  (define (draw-sides-right-help n)
    (display " ")
    (display "\u2502")
    
    (if (> n 1)
        (draw-sides-right-help (- n 1)) )
    )
  
  (if (> n 0)
      (draw-sides-right-help n) )
  (display "\n")
  )

(define (draw-lines n)
  (define (draw-lines-help n)
    (display "\u2500")
    
    (if (> n 1)
        (draw-lines-help (- n 1)) )
    )
 (draw-lines-help (+ 1 (* 4 (- n 1))))
  )

(define (draw-top n)
  (define (draw-top-help curr n)
    (draw-sides-left(- n curr) )
    (display "\u250c")
    (draw-lines curr)
    (display "\u2510")
    (draw-sides-right (- n curr) )
    
    (if (> curr 1)
        (draw-top-help (- curr 1) n) )
    )
  
  (if (> n 0)
      (draw-top-help n n) )
  )

(define (draw-bottom n)
  (define (draw-bottom-help curr n)
    (draw-sides-left (- n curr) )
    (display "\u2514")
    (draw-lines curr )
    (display "\u2518")
    (draw-sides-right (- n curr) )
    
    (if (< curr n)
        (draw-bottom-help (+ curr 1) n) )
    )
  
  (if (> n 0)
      (draw-bottom-help 1 n) )
  )

(define (squares n)
  (draw-top n)
  (draw-bottom n)
  )

(squares 3)