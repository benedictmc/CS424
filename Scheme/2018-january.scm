(define (tear x y)  
    (if (null? y) (list ) ;if null
        (list (tear-t x y)  (tear-f x y))))

(define (tear-t x y)  
    (if (null? y) (list ) 
        (cond 
            [(x (car y)) (cons (car y) (tear-t x (cdr y)))] 
            [else (tear-t x (cdr y)) ] 
    )))

(define (tear-f x y)  
    (if (null? y) (list ) ;if null
        (cond 
            [(not (x (car y))) (cons (car y) (tear-f x (cdr y)))] 
            [else (tear-f x (cdr y)) ] 
    )))


(tear number? '(a b c 1 2 3 d e f))