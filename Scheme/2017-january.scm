(define (foo x y) ;define function
    (if (null? y) (list ) ;if null
      (cond 
        [(list? (car y)) (append (foo x (car y)) (foo x (cdr y)))] 
        [(x (car y)) (cons (car y) (foo x (cdr y)))] 
        [else (foo x (cdr y)) ] 
      )))
      
      
(foo number? '(a (2 (c 3) 4 1 4 5 2)))