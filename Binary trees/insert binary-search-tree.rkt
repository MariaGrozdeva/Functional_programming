#lang racket/base

(define (insert binary-search-tree element)
  (if (null? binary-search-tree)
      (list element null null)
      (if (< element (car binary-search-tree))
          (list (car binary-search-tree) (insert (cadr binary-search-tree) element) (caddr binary-search-tree))
          (list (car binary-search-tree) (cadr binary-search-tree) (insert (caddr binary-search-tree) element)) ) )
  )