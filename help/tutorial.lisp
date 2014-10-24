#| HSC3-LISP |#

; AMERICAN PRIMITIVE, VOL. 2
; λ, MACRO, SET!, IF, QUOTE, CONS

#| EMACS LISP |#

; The RSC3-MODE more or less works.  Type:
;
; (setq rsc3-interpreter (list "hsc3-lisp"))
;
; C-\    = lambda
; M-\    = λ
; C-cC-a = (hear <point>)
; C-cC-g = (draw <point>)
; C-cC-k = (reset)

#| CHURCH LISP |#

; Functions and procedures are of the form λ α → β.

((λ n ((* n) n)) 3) ; 9

+ ; (λ a (λ b (PRIM:+ (cons a b))))
(+ 1) ; (λ b (PRIM:+ (cons 1 b)))
((+ 1) 2) ; 3

; The evaluator allows two notational simplifications.
; The form (f p q) is read as ((f p) q) and so on.
; The form (f) is read as (f nil).

(+ 1 2) ; 3
((λ _ 1)) ; 1

; Single argument λ is against the grain of traditional variadic notation.

(+ 1 2 3) ; ERROR: (3 3)

#| CELLULAR LISP |#

; The CONS cell is the primitive composite value.

(cons 1 2) ; (1 . 2)

; CONS is undone with CAR and CDR.

(car (cons 1 2)) ; 1
(cdr (cons 1 2)) ; 2

#| QUOTING LISP |#

; QUOTE protects an S-EXPRESSION from EXPAND and EVAL.

(quote (+ 1 2)) ; (+ 1 2)

; EVAL is UNQUOTE.

(eval (quote (+ 1 2))) ; 3

#| REWRITING LISP |#

; MACROS are programs that re-write S-EXPRESSION programs.

((λ exp (cons '- (cdr exp))) '(+ 1 2)) ; (- 1 2)

; The MACRO form takes an S-EXPRESSION re-writing program.
; When applied the MACRO receives the remainder of expression as a LIST.

((macro (λ exp (cons '- (cdr exp)))) + 1 2) ; -1

; MACROS may expand to MACROS.

b ; ERROR: ENV-LOOKUP
(set! a (macro (λ exp (list 'set! 'b (car exp))))) ; NIL
(a 5) ; NIL
b ; 5

; EXPAND expands a form where the LHS is a MACRO.

(expand '(a 5)) ; (set! b 5)

; DEFINE, LAMBDA, LET, AND, OR, COND, BEGIN, WHEN, and LIST are all MACROS.

#| MUTATING LISP |#

; SET! is the primitive environment editor.
; SET! creates a new entry at the TOP-LEVEL if the variable is not otherwise located.

(set! a nil) ; NIL
a ; NIL
(set! b (λ _ a)) ; NIL
(b) ; NIL
(set! a 'a) ; NIL
(b) ; a

#| CONDITIONAL LISP |#

; IF THEN ELSE is the primitive conditional.
; The only false value is #f, all other values are #t.

(if #t 'a 'b) ; a
(if #f (print 'a) (print 'b)) ; b NIL
(if 'false 'true 'false) ; true

; IF requires both true and false branches, see WHEN for alternate.

(if #t 'true) ; ERROR

#| EVALUATING LISP |#

(eval 1) ; 1
(eval (eval 1)) ; 1

(1) ; ERROR

#| VARIADIC LISP |#

; MACROS can implement variable argument functions.

list ; MACRO
(list) ; nil
(list 1 2 3) ; (1 2 3)

; The standard MACROS also define the associated re-writer.

list-rw ; LAMBDA
(list-rw (cdr '(list))) ; NIL
(list-rw (cdr '(list 1 2 3))) ; (cons 1 (cons 2 (cons 3 nil)))

#| STANDARDISED LISP |#

(map (+ 1) (list 1 2 3)) ; (2 3 4)
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

; BEGIN cannot be elided.

((lambda (p q) (display p) (print q))) ; ERROR
((lambda (p q) (begin (display p) (print q))) 1 2) ; 12

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

(let ((set! 0) (set! 1)) set!) ; 1

; LET is unary

(let ((a 1)) (display a) (newline)) ; ERROR
(let ((a 1)) (begin (display a) (newline))) ; 1\n

; LET is schemes LET*.

(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))) ; 70 (NOT 35)

(letrec-rw (cdr '(letrec ((a 5) (b 6)) (cons a b))))
; (let ((a NIL) (b NIL)) (begin (set! a 5) (set! b 6) (cons a b)))

(define add-count
  (lambda (l)
    (letrec ((f (lambda (n l) (if (null? l) '() (cons (cons n (car l)) (f (+ n 1) (cdr l)))))))
      (f 0 l))))

(add-count (list 'a 'b 'c)) ; ((CONS 0 a) (CONS 1 b) (CONS 2 c))

; CONS LISP

(define c (cons 1 2)) ; NIL
(car c) ; 1
(cdr c) ; 2
(pair? c) ; #t
(list? c) ; #f
(null? c) ; #f
(null? '()) ; #t

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

; RANDOM LISP

(i-random 0 3)
(replicate-m 12 (choose (list 1 2 3)))

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

(load "/home/rohan/sw/hsc3-lisp/lisp/stdlib.lisp")

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

(reset nil)
(draw (* (sin-osc ar 440 0) 0.1))
(draw (* (sin-osc ar (mouse-x kr 440 880 0 0.1) 0) 0.1))
(draw (* (hpz1 (white-noise ar)) 0.1))
(display-server-status nil)
(hear (* (sin-osc ar 440 0) 0.1))

; INSENSITIVE LISP

(hear (MUL (SIN-OSC AR 440 0) 0.1))

; UID

(set! uid 0) ; NIL
(map incr-uid '(1 1 1)) ; (1 2 3)

#| DERIVATIVE CONS |#

; CONS need not be primitive, it can be in terms of λ.

(define kons (λ x (λ y (λ m (m x y)))))
(define kar (λ z (z (λ p (λ q p)))))
(define kdr (λ z (z (λ p (λ q q)))))

(kons 1 2) ; (λ m (m x y))
(kar (kons 1 2)) ; 1
(kdr (kons 1 2)) ; 2

#| Y |#

(define length*
  ((λ h (h h))
   (λ g (λ l (if (null? l) 0 ((+ 1) ((g g) (cdr l))))))))

(length* (list 1 3 5 7 9)) ; 5

(define y (λ w ((λ f (f f)) (λ f (w (λ x ((f f) x)))))))

(define length** (y (λ f (λ l (if (null? l) 0 (+ 1 (f (cdr l))))))))

(length** (list 1 3 5 7 9)) ; 5

(define maximum*
  (y (λ f (λ l (cond ((null? l) -inf)
                     ((> (car l) (f (cdr l))) (car l))
                     (else (f (cdr l))))))))

(maximum* (list 1 5 9 7 3)) ; 9
(maximum* (list -5 -7 -3)) ; -3
