#| HSC3-LISP |#

; AMERICAN PRIMITIVE, VOL. 2
; λ, MACRO, SET!, IF, QUOTE/EVAL, CONS

#| EMACS LISP |#

; (setq rsc3-interpreter (list "hsc3-lisp"))

#| CHURCH LISP |#

; Functions and procedures are of the form λ α → β.

((λ n ((* n) n)) 3) ; 9

+ ; (λ a (λ b (PRIM:+ (cons a b))))
(+ 1) ; (λ b (PRIM:+ (cons 1 b)))
((+ 1) 2) ; 3

; The evaluator allows two notational simplifications.

(+ 1 2) ; 3
((λ _ 1)) ; 1

; This notation disallows the traditional variadic notation.

(+ 1 2 3) ; ERROR: (3 3)

#| CELLULAR LISP |#

; The CONS cell is the primitive combining type, it is undone with CAR and CDR.

(cons 1 2) ; (1 . 2)
(car (cons 1 2)) ; 1
(cdr (cons 1 2)) ; 2

#| QUOTING LISP |#

; QUOTE protects an S-EXPRESSION from EVAL

(quote (+ 1 2)) ; (+ 1 2)

; EVAL is UNQUOTE.

(eval (quote (+ 1 2))) ; 3

#| REWRITING LISP |#

; A MACRO is a program that re-writes programs.

((λ exp (cons '- (cdr exp))) '(+ 1 2)) ; (- 1 2)

; A MACRO receives the remainder of expression to which it is applied as a LIST.

((macro (λ exp (cons '- (cdr exp)))) + 1 2) ; -1

; DEFINE, LAMBDA, LET, AND, OR, COND, BEGIN, WHEN, and LIST are all MACROS.

#| MUTATING LISP |#

; SET! is the primitive environment editor.

(set! a nil) ; NIL
a ; NIL
(set! a 5) ; NIL
a ; 5

(set! b (λ _ a)) ; NIL
(b) ; 5
(set! a 4) ; NIL
(b) ; 4

#| CONDITIONAL LISP |#

(if #t 'a 'b) ; a
(if #f 'a 'b) ; b

(set! t #t) ; NIL
(if t 'a 'b) ; a

#| EVALUATING LISP |#

(eval 1) ; 1
(eval (eval 1)) ; 1

(1) ; ERROR

#| VARIADIC LISP |#

; MACROS can implement variable argument functions.

list ; MACRO
(list 1 2 3) ; (1 2 3)

; The standard MACROS also define the associated re-writer.

list-rw ; LAMBDA
(list-rw (cdr '(list 1 2 3))) ; (cons 1 (cons 2 (cons 3 nil)))

#| STANDARDISED LISP |#

(map (compose (+ 1) (* 2)) (list 1 2 3)) ; (3 5 7)
(map (compose (/ 2) (+ 3)) (list 1 2 3)) ; (1/2 2/5 1/3)
(map (const 3) (list 1 2 3)) ; (3 3 3)
(cons (- 1 2) ((flip -) 1 2)) ; (cons -1 1)
(id 1) ; 1

(procedure? +)

; There is a MACRO, lambda, that approximates the SCHEME form.

(lambda-rw (cdr '(lambda () x))) ; (λ _ x)
(lambda-rw (cdr '(lambda (x) x))) ; (λ x x)
(lambda-rw (cdr '(lambda (x y) (cons x y)))) ; (λ x (λ y (cons x y)))
(lambda-rw (cdr '(lambda (x y z) (list x y z)))) ; (λ x (λ y (λ z (list x y z))))

((lambda () 1) nil) ; 1
((lambda (n) (* n n)) 3) ; 9
((lambda (x y z) (+ x (+ y ((lambda (n) (* n n)) z)))) 1 2 3) ; 12

; NIL LISP

nil ; NIL
(null? nil) ; #t

; EQ LISP

(equal? 'a 'a) ; #t
(equal? "b" "b") ; #t
(= 5 5) ; #t

; ORD LISP

(< 0 1) ; #t
(> 0 1) ; #f
(min 1 2) ; 1
(max 1 2) ; 2
(compare 1 2) ; 'lt
(compare 2 1) ; 'gt
(compare 1 1) ; 'eq

; SEQUENTIAL LISP

(begin-rw (cdr '(begin))) ; NIL
(begin-rw (cdr '(begin (print 1)))) ; ((λ _ (print 1)) nil)
(begin-rw (cdr '(begin (print 1) (print 2)))) ; ((λ _ (print 2)) ((λ _ (print 1)) nil))

(begin (print 1) (print 2) (print 3))
((λ _ (print 3)) ((λ _ (print 2)) ((λ _ (print 1)) nil)))

((λ x (begin (display x) (set! x 5) (print x))) 0) ; 05

#| DEFINING LISP |#

; DEFINE is an alias for set!

(define-rw (cdr '(define one 1))) ; (set! one 1)

(define one 1) ; NIL
one ; 1

(define sq (λ n ((* n) n))) ; NIL
(sq 5) ; 25

(define sum-sq (lambda (p q) (+ (sq p) (sq q)))) ; NIL
(sum-sq 7 9) ; 130

not-defined ; ERROR
((lambda (_) (define not-defined 1)) nil) ; NIL
not-defined ; 1

; BINDING LISP

(let-rw (cdr '(let () 1))) ; 1
(let-rw (cdr '(let ((a 5)) (+ a 1)))) ; ((λ a (+ a 1)) 5)
(let-rw (cdr '(let ((a 5) (b 6)) (+ a b)))) ; ((λ a ((λ b (+ a b)) 6)) 5)

(let ((a 5)) a) ; 5
(let ((a 5) (b 6)) (cons a b)) ; (cons 5 6)
(let ((a 5) (b (+ 2 3))) (* a b)) ; 25
(let ((a 5) (b (+ a 3))) (* a b)) ; 40

; LET is schemes LET*.

(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))) ; 70 (NOT 35)

; CONS LISP

(define c (cons 1 2)) ; NIL
(car c) ; 1
(cdr c) ; 2
(pair? c) ; #t
(list? c) ; #f
(null? c) ; #f
(null? '()) ; #t

; LIST LISP

(list-rw (cdr '(list))) ; NIL
(list-rw (cdr '(list 1 2 3))) ; (cons 1 (cons 2 (cons 3 nil)))
(list 1 2 3) ; (1 2 3)
list ; MACRO

(define l (cons 1 (cons 2 (cons 3 '())))) ; NIL
(null? l) ; #f
(pair? l) ; #t
(list? l) ; #t
(length l) ; 3
(append l l) ; (1 2 3 1 2 3)

(append '(1 2 3) (list 4 5 6)) ; (1 2 3 4 5 6)
(foldl + 0 (list 1 2 3)) ; 6
(foldr cons '() (list 1 2 3)) ; (1 2 3)
(list 1 2 3) ; (1 2 3)
(list 1 2 ((lambda (_) (cons 3 4)) nil) 5) ; (1 2 (cons 3 4) 5)
(map (+ 1) (list 1 2 3)) ; (2 3 4)
(maximum '(1 3 5 4 2 0)) ; 5
(minimum '(1 3 5 4 2 0)) ; 5
nil ; '()
(nub '(1 2 2 3 3 3)) ; (1 2 3)
(nub-by (on equal? car) '((0 1) (0 2) (1 2))) ; ((0 1) (1 2))
(not-elem 1 '(0 2 4))
(reverse (list 1 2 3)) ; (3 2 1)

; TREE LISP

(flatten '(1 2 (3 4 (5)) 6)) ; (1 2 3 4 5 6)
(levels '(1 2 (3 4 (5)) 6)) ; ((1 2 6) (3 4) (5))

; PRELUDE LISP

(enum-from-difference-to > 0 1 9) ; (0 1 2 3 4 5 6 7 8 9)
(enum-from-difference-to < 0 -3 -9) ; (0 -3 -6 -9)
(enum-from-then-to 0 1 9) ; (0 1 2 3 4 5 6 7 8 9)
(enum-from-then-to 0 -3 -9) ; (0 -3 -6 -9)
(enum-from-to 0 9) ; (0 1 2 3 4 5 6 7 8 9)
(map signum '(-3 0 3)) ; (-1 0 1)
(succ 1) ; 2
(pred 2) ; 1

; LOGICAL LISP

(not #t) ; #f
(not #f) ; #t
(not 'SYM) ; ERROR

(and-rw (cdr '(and p q))) ; (if p q 0)
(list (and #t #t) (and #t #f) (and #f #t) (and #f #f)) ; (#t #f #f #f)

(or-rw (cdr '(or p q))) ; (if p #t q)
(list (or #t #t) (or #t #f) (or #f #t) (or #f #f)) ; (#t #t #t #f)

(cond-rw (cdr '(cond))) ; NIL
(cond-rw (cdr '(cond (a b)))) ; (if a b nil)
(cond-rw (cdr '(cond (a b) (c d)))) ; (if a b (if c d nil))
(cond-rw (cdr '(cond (a b) (c d) (else e)))) ; (if a b (if c d e))
(cond-rw (cdr '(cond ((> x y) 'gt) ((< x y) 'lt) (else 'eq))))

(when-rw (cdr '(when a b))) ; (if a b NIL)
(when #t (print 'TRUE)) ; TRUE
(when #f (print 'FALSE)) ; NIL

(when ((lambda (_) #t) nil) (print 'TRUE)) ; TRUE
(when ((lambda (_) #f) nil) (print 'FALSE)) ; NIL

; MATHEMATICAL LISP

; Binary operator UGens are optimising.

(add 1 2) ; 3

; Symbolic aliases are given.

(+ 1 2) ; 3

; Constants are numbers.

(number? 1) ; #t
(number? 'one) ; #f
(number? (sin-osc kr 5 0)) ; #f

; CONTROL.MONAD LISP

(replicate-m-rw (cdr '(replicate-m 4 (rand 0 1)))) ; (replicate-m* 4 (lambda (_) (rand 0 1)))
(replicate-m 4 (rand 0 1)) ; (Rand Rand Rand Rand)


; TEMPORAL LISP

(begin (print 'BEFORE) (pause-thread 1) (print 'AFTER))

(utcr) ; <real>

(let ((t (utcr)))
  (begin
    (print 'BEFORE)
    (pause-thread-until (+ t 1))
    (print 'AFTER)))

(define random-sine (mul (sin-osc ar (rand 220 440) 9) 0.01))
(dt-rescheduler (lambda (t) (begin (hear random-sine) 1)) (utcr))

; IO LISP

newline-char ; 10
(write-char newline-char)
(newline) ; \n
(print 1) ; 1
(print (+ 1 2)) ; 3
(begin (display 1) (print 2)) ; 12
(define three (begin (display* 1) (print 2) 3)) ; 1 2 NIL
three ; 3

; STRING LISP

"string" ; "string"
(string? "string") ; #t

; LOADING LISP

(load "/home/rohan/sw/hsc3-forth/lisp/stdlib.lisp")

; FLOATING LISP

(map sin (enum-from-then-to 0 0.05 pi))

; SICP

(define square (lambda (n) (* n n))) ; NIL

(define f
  (lambda (x y)
    ((lambda (a b) (+ (+ (* x (square a)) (* y b)) (* a b)))
     (+ 1 (* x y))
     (- 1 y))))

(f 7 9) ; 28088

; UGEN

(stop nil)
(draw (* (sin-osc ar 440 0) 0.1))
(draw (* (sin-osc ar (mouse-x kr 440 880 0 0.1) 0) 0.1))
(draw (* (hpz1 (white-noise ar)) 0.1))
(sc3-status nil)
(play (* (sin-osc ar 440 0) 0.1))

; UID

(set! uid 0) ; NIL
(map incr-uid '(1 1 1)) ; (1 2 3)

#| CHURCH CONS |#

; CONS need not be primitive, it can be in terms of λ.

(define kons (λ x (λ y (λ m (m x y)))))
(define kar (λ z (z (λ p (λ q p)))))
(define kdr (λ z (z (λ p (λ q q)))))

(kons 1 2) ; (λ m (m x y))
(kar (kons 1 2)) ; 1
(kdr (kons 1 2)) ; 2

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
