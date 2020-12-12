(define (ascending? n)
  (if (< n 10)
      #t
      (and (> (modulo n 10) (modulo (quotient n 10) 10)) (ascending? (quotient n 10)))))


(define (nset-filter s pred?) (set-filter-helper s pred? 0 0))

(define (set-filter-helper s pred? res ind)
  (if (= s 0)
      res
  (if (= (modulo s 2) 0)
      (set-filter-helper (quotient s 2) pred? res (+ 1 ind))
      (if (pred? ind)
          (set-filter-helper (quotient s 2) pred? ( + (expt 2 ind) res) (+ 1 ind))
          (set-filter-helper (quotient s 2) pred? res (+ 1 ind))))))



(define (contains? s el) (not (= (nset-filter s (lambda (x) (= x el)) ) 0)))
  

  (define (nset-intersect s1 s2) (nset-filter s1 (lambda(x) (contains? s2 x))))


  ;;recursivno.. ne prochetoh uslovieto. Gore e pravilnoto
 (define (nset-intersect-rec s1 s2) (nset-intersect-helper s1 s2 0 0))

(define (nset-intersect-helper s1 s2 res ind)
  (if (or (= s1 0) (= s2 0))
      res
      (if (and (= (modulo s1 2) 1) (= (modulo s2 2) 1))
          (nset-intersect-helper (quotient s1 10) (quotient s2 10) ( + res (expt 2 ind)) (+ ind 1))
          (nset-intersect-helper (quotient s1 10) (quotient s2 10)  res (+ ind 1)))))