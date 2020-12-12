#lang racket/base

(require rackunit rackunit/gui)
(require "tree.rkt")



(define (expect-correct tree-expr)
  (test-true
   (string-append "Tree-expr " tree-expr " should be correct")
   (tree? tree-expr)
   )
  )
(define (expect-incorrect tree-expr)
  (test-false
   (string-append "Tree-expr " tree-expr " should be incorrect")
   (tree? tree-expr)
   )
  )
(define (expect-false-on-incorrect-syntax tree-expr)
  (test-false
   (string-append "Tree-expr " tree-expr " should have incorrect syntax")
   (check-syntax tree-expr)
   )
  )


(define (expect-height tree h)
  (test-equal?
   (string-append "Expecting height of " (number->string h) " for tree" (tree->string tree) )
   (height tree)
   h
   )
  )
(define (expect-balanced tree)
  (test-true
   (string-append "Tree " (tree->string tree) " should be balanced")
   (balanced? tree)
   )
  )
(define (expect-not-balanced tree)
  (test-false
   (string-append "Tree " (tree->string tree) " should not be balanced")
   (balanced? tree)
   )
  )


(define (expect-ordered tree)
  (test-true
   (string-append "Tree " (tree->string tree) " should be ordered")
   (ordered? tree)
   )
  )
(define (expect-not-ordered tree)
  (test-false
   (string-append "Tree " (tree->string tree) " should not be ordered")
   (ordered? tree)
   )
  )



(test/gui
 
  (test-suite
   "remove-whitespaces works correctly"

   (test-equal?
    "Expressions without whitespace are left intact"
    (remove-whitespaces "{2{4**}*}")
    "{2{4**}*}")

   (test-equal?
    "Whitespaces are removed"
    (remove-whitespaces "{ 2     {4     * * }               *}")
    "{2{4**}*}")
   )


  (test-suite
   "check-syntax returns #f for incorrect synax"
   (expect-false-on-incorrect-syntax "5{{1**}")
   (expect-false-on-incorrect-syntax "{5{1**}}}")
   )


  (test-suite
  "tree?"
  (expect-correct "*")
  (expect-correct "   * ")
  (expect-correct "{2     * *}")
  (expect-correct "{2 {4 * *} *}")
  (expect-correct "{2{4**}*}")
  (expect-correct "{ 2     {4     * * }               *}")
  (expect-incorrect "{}")
  (expect-incorrect "{*}")
  (expect-incorrect "{1 2 3}")
  (expect-incorrect "{5 {   22  {2 *  *}   {6 * *}}")
  (expect-incorrect "{5 {{22  2 *  *} {6   * *}} {1 * {3 {111 *   *} *} }}")
  (expect-incorrect "{5 {22 {2 * *} {6 * *} } {1 {3 {111   * *} *}  } }")
  )


  (test-suite
   "string->tree"
   (test-equal?
    "String is converted to tree"
    (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")
    '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))
    )
   (test-equal?
    "String is converted to tree"
    (string->tree "{5 **}")
    '(5 () ())
    )
   (test-equal?
    "String is invalid tree --> string->tree returns #f"
    (string->tree "{5 {   22  {2 *  *}   {6 * *}}")
    #f
    )
   )


  (test-suite
   "Evaluating the height of a tree correctly"
   (expect-height '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))) 3)
   (expect-height '(5 (22 (2 () ()) (6 () ())) (1 () (3 () ()))) 2)
   (expect-height '(1 (2 () ()) ()) 1)
   (expect-height '(11 () ()) 0)
   )


  (test-suite
   "balanced?"
   (expect-balanced '(5 (22 (2 () ()) (6 () ())) (1 () (3 () ()))))
   (expect-balanced '(5 (22 () ()) ()))
   (expect-not-balanced '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))
   (expect-not-balanced '(23 (22 (5 () ())) ()))
   )


  (test-suite
   "ordered?"
   (expect-ordered '(4 (2 () ()) (6 () ())))
   (expect-ordered '(4 (2 () (3 ()())) (6 () (7 () (9 () ()) ))))
   (expect-not-ordered '(4 (2 () (5 ()())) (6 () ())))
   (expect-not-ordered '(4 (2 () (5 ()())) (6 ( 3 () () ) ())))
   )


  (test-suite
   "tree->string"
   (test-equal?
    "Tree is converted to string"
    (tree->string '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))
    "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"
    )
  (test-equal?
   "Tree is converted to string"
   (tree->string '(4 (2 () (5 ()())) (6 () ())))
   "{4 {2 * {5 * *}} {6 * *}}"
   )
  )

  )