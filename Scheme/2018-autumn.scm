(define (map-skip x y) ;define function
    (if (null? y) (list ) ) ;if null
      (if (null? (cdr y)) (cons (x (car y)) (map-skip x (cdr y)))  ;If odd
        (cons (x (car y)) (cons (cadr y) (map-skip x (cddr y))))))   ;else (even)
  
  
  (map-skip (lambda (x) (+ x 1000)) '(1 2 3 4 5 6 7))
  