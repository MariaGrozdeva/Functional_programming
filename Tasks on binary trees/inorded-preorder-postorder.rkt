#lang racket/base

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