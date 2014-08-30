; R5RS

; 2.2

(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
(map fact (enum-from-to 0 12))

; 4.1.3

(+ 3 4) ; 7
((if #f + *) 3 4) ; 12

; 4.1.4

(lambda (x) (+ x x)) ; (λ x (+ x x))
((lambda (x) (+ x x)) 4) ; 8
(define reverse-subtract (lambda (x y) (- y x))) ; NIL
(reverse-subtract 7 10) ; 3
(define add4 (let ((x 4)) (lambda (y) (+ x y)))) ; NIL
(add4 6) ; 10

; 4.1.5

(if (> 3 2) 'yes 'no) ; yes
(if (> 2 3) 'yes 'no) ; no
(if (> 3 2) (- 3 2) (+ 3 2)) ; 1

; 4.1.6

(define x 2) ; NIL
(+ x 1) ; 3
(set! x 4) ; NIL
(+ x 1) ; 5

; 4.2.1

(cond ((> 3 2) 'greater) ((< 3 2) 'less)) ; greater
(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)) ; equal

(and (= 2 2) (> 2 1)) ; #t
(and (= 2 2) (< 2 1)) ; #f

(or (= 2 2) (> 2 1)) ; #t
(or (= 2 2) (< 2 1)) ; #t

; 4.2.2

(let ((x 2) (y 3)) (* x y)) ; 6

(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))) ; 70

(define zero? (lambda (n) (equal? n 0)))

(letrec ((even?
          (lambda (n)
            (if (zero? n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                #f
                (even? (- n 1))))))
  (even? 89))

; 4.2.3

(define x 0) ; NIL
(begin (set! x 5) (+ x 1)) ; 6
(begin (display "4 plus 1 equals ") (display (+ 4 1))) ; 4 plus 1 equals 5

; 5.2.1

(define add3 (lambda (x) (+ x 3))) ; NIL
(add3 3) ; 6
(define first car) ; NIL
(first '(1 2)) ; 1

; 6.1

(equal? 'a 'a) ; #t
(equal? '(a) '(a)) ; #t
(equal? '(a (b) c) '(a (b) c)) ; #t
(equal? "abc" "abc") ; #t
(equal? 2 2) ; #t
(equal? (lambda (x) x) (lambda (y) y)) ; #f

; 6.2.5

(max 3 4) ; 4
(max 3.9 4) ; 4

(+ 3 4) ; 7

(define +: (macro (lambda (exp) (foldl + 0 exp))))
(define *: (macro (lambda (exp) (foldl * 1 exp))))

(+: 3) ; 3
(+:) ; 0
(*: 4) ; 4
(*:) ; 1

(define ceiling ceil)
(define round* (lambda (n) (round n 1)))

(floor -4.3) ; -5
(ceiling -4.3) ; -4
(round* -4.3) ; -4

(floor 3.5) ; 3
(ceiling 3.5) ; 4
(round* 3.5) ; 4

; 6.3.1

(not #t) ; #f
(not 3) ; #f
(not (list 3)) ; ?
(not #f) ; #t
(not '()) ; ?
(not (list)) ; ?
(not 'nil) ; ?
