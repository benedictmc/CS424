; Author: Benedict McGovern
; Student Number: 15340696

; POLY EVAL 
(define poly-eval (lambda (p x) (for/fold ([res 0]) ([i (reverse p)]) (+ (* res x) i))))

(define (reverse lst)
    (if (null? lst)
        lst
        (append (reverse (cdr lst)) (list (car lst)))))
; ***************************

;  POLY ADD
(define (poly-add lst1 lst2)
  (let ((diff (- (length lst1) (length lst2))))
    (cond [(< diff 0)
           (map + (fill-zeroes lst1 diff) lst2)]
          [(> diff 0)
           (map + lst1 (fill-zeroes lst2 diff))]
          [else (map + lst1 lst2)])))

(define (fill-zeroes lst n)
  (append lst (make-list (abs n) 0)))
        
(define (fill-ones lst n)
  (append lst (make-list (abs n) 1)))
; ***************************

;  POLY MUL
(define (poly_mul apol bpol)
  (revlist(zero_trim(revlist(poly_mull apol  bpol)))))

(define (poly_mull apol bpol)
  (if (null? bpol)
      (zero_add 1)
         (mul_add (int_pol_mult (car bpol) apol) (poly_mull(change_deg 1 apol) (cdr bpol))))) 

(define (mul_add apol bpol)
  (if (null? apol)
      bpol
      (if (null? bpol)
          apol
          (cons (+ (car apol) (car bpol))(mul_add (cdr apol) (cdr bpol))))))

(define (int_pol_mult x apol)
  (if(null? apol)
      (list )
      (cons (* x (car apol))(int_pol_mult x (cdr apol)))))
(define (change_deg x apol)
  (append (zero_add x) apol))

(define (zero_add x)
  (if (< x 1)
      (list )
      (cons 0 (zero_add (- x 1)))))
; ***************************

; POLY DIFF
(define (poly-diff p) (diff-helper p '() 0))

(define diff-helper
  (lambda (L result count)
    (if (null? L) result
        (cond ((= count 0) (diff-helper (cdr L) result (+ 1 count)))
              ((= (car L) 0) (diff-helper (cdr L) (append result (list 0)) (+ 1 count)))
              (else
               (diff-helper (cdr L) (append result (list (* (car L) count))) (+ 1 count)))))))
; ***************************

; POLY INT
(define (poly-int p) (int-helper p '(0) 1))

(define int-helper
  (lambda (L result count)
    (if (null? L) result
        (cond ((= (car L) 0) (int-helper (cdr L) (append result (list 0)) (+ 1 count)))
              (else
              (int-helper (cdr L) (append result (list (/ (car L) count))) (+ 1 count)))))))
; ***************************

; GROVEL POLY EVAL
(define (flat s)
  (cond ((null? s) (list ))
        ((pair? (car s)) (append (flat (car s)) (flat (cdr s))))

        ((equal? (car s) 'poly) (append  (list (poly-eval (cdr s) 5)) (flat (list (cdr s)))))
  (else (cons (car s) (flat (cdr s))))))


(define (govel-poly-eval s x) (flat s))

; ***************************

; RUN TESTS
(define (run-tests) 
  (display "run-tests method:")(newline)
  (display "****Poly-Eval:**** ")(newline)
  (display "poly-eval: (poly-eval '(1 2 3) 100) = ")
  (display (poly-eval '(1 2 3) 100))(newline)
  (display "poly-eval: (poly-eval '() 7) = ")
  (display (poly-eval '() 7))(newline)
  (display "****Poly-Add:**** ")(newline)
  (display "(poly-add '(1 2 3) '(11 21 31 41)) = ")
  (display (poly-add '(1 2 3) '(11 21 31 41)))(newline)
  (display "Poly-mul: ")(newline)
  (display "(poly-mul '(1 2 3) '(11 21 31 41))= ")
  (display (poly_mull '(1 2 3) '(11 21 31 41)))(newline)
  (display "****Poly-Diff:**** ")(newline)
  (display "(poly-diff '(1 10 100 1000 10000)) = ")
  (display (poly-diff '(1 10 100 1000 10000)))(newline)
  (display "****Poly-Int:***** ")(newline)
  (display "(poly-int '(10 200 3000 40000)) = ")
  (display (poly-int '(10 200 3000 40000)))(newline)
  (display "****Grovel-Poly-Eval:***** ")(newline)
  (display "(govel-poly-eval '(the (poly 1 2) brown (fox (poly 7 0 1) jumps (poly) over)) 5) = ")(newline)
  (display (govel-poly-eval '(the (poly 1 2) brown (fox (poly 7 0 1) jumps (poly) over)) 5))(newline)
)
; (govel-poly-eval '(the (poly 1 2) brown (fox (poly 7 0 1) jumps (poly) over)) 5)

; ***************************
(run-tests)