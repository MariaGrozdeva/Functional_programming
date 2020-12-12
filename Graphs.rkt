#lang racket

; Задача 1: Да се дефинира функция (nodes edges),
; която приема списък с ребрата edges на даден ориентиран граф
; (в който всяко ребро е представено като двойка (from . to))
; и връща списък, съдържащ всички върхове на съответния граф. 
 (define (push-unique elem list)
   (if (member elem list)
       list
       (cons elem list) )
   )

(define (nodes edges)
  (foldl (lambda (edge list-of-nodes)
           (push-unique (cdr edge) (push-unique (car edge) list-of-nodes))
           )
         null
         edges
         )
  )
; (nodes (list (cons 1 2) (cons 2 3) (cons 1 4)
            ; (cons 5 6) (cons 2 4) (cons 3 5)))

; Задача 2: Да се дефинира функция (adjacency-list edges),
; която приема списък с ребрата edges на даден ориентиран граф
; (в който всяко ребро е представено като двойка (from . to))
; и връща списъка на наследниците на съответния граф.
(define (adjacency-list edges)
  (define (get-nodes start)
    (cons start (list (map cdr (filter (lambda (edge)
                                         (= (car edge) start))
                                       edges)
                           )
                      )
          )
    )
  (map get-nodes (nodes edges))
  )
; (adjacency-list (list (cons 1 2) (cons 2 3) (cons 1 4)
           ; (cons 5 6) (cons 2 4) (cons 3 5)))

; Задача 3: Да се дефинира функция (path? edges nodes),
; която приема списък с ребрата edges на даден
; ориентиран граф и списък от върхове nodes и връща
; дали списъкът nodes е път в графа описан от edges.
(define (drop-last l)
  (cond [(null? l) '()]
        [(null? (cdr l)) '()]
        [else (cons (car l) (drop-last (cdr l)))]
        )
  )

(define (path? edges nodes)
  (define path-edges (map cons (drop-last nodes) (cdr nodes)))
  (define (bin-and x y) (and x y))
  (foldl bin-and #t (map (lambda (path-edge) (if (member path-edge edges) #t #f)) path-edges))
  )
; (path? (list (cons 1 2) (cons 2 3) (cons 1 4)
           ; (cons 5 6) (cons 2 4) (cons 3 5)) '(1 2 3))