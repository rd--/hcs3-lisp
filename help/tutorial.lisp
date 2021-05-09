; hsc3-lisp
; ---------

; primitives are: λ, macro, set!, if, quote, fork, cons
λ ; error: env-lookup: λ

; Emacs
; -----

; rsc3-mode more or less works.  Type:
;
; (setq rsc3-interpreter (list "hsc3-lisp"))
;
; C-\    = lambda
; M-\    = λ
; C-cC-a = play graph
; C-cC-g = draw graph
; C-cC-k = reset scsynth

; Lambda
; ------

; Functions and procedures are of the form λ α → β.

((λ n ((* n) n)) 3) ; 9

+ ; (λ a (λ b (mk-ugen...)))
(+ 1) ; (λ b (mk-ugen...))
((+ 1) 2) ; 3

; The evaluator allows two notational simplifications.
; The form (f p q) is read as ((f p) q) and so on.
; The form (f) is read as (f nil).

(+ 1 2) ; 3
((λ _ 1)) ; 1

; Single argument λ is against the grain of traditional variadic notation.

(+ 1 2 3) ; error: (3 3)

; Cons
; ----

; The cons cell is the primitive composite value.

(cons 1 2) ; (cons 1 2)

; cons cell elements are accessed using car and cdr.

(car (cons 1 2)) ; 1
(cdr (cons 1 2)) ; 2

; Predicates are:

(pair? c) ; #t
(list? c) ; #f
(null? c) ; #f
(null? nil) ; #t
(null? '()) ; #t

; List
; ----

(list 1 2 3) ; (1 2 3)
(take 2 (list 1 2 3)) ; (1 2)
(list-ref (list 1 2 3) 1) ; 2 ; zero-indexed

; Quote
; -----

; quote protects an s-expression from expand and eval.

(quote (+ 1 2)) ; (+ 1 2)

; 'x is (quote x)
'(+ 1 2) ; (+ 1 2)

; eval is unquote.

(eval (quote (+ 1 2))) ; 3

; Macros
; ------

; Macros are programs that re-write s-expression programs.

((λ exp (cons '- (cdr exp))) '(+ 1 2)) ; (- 1 2)

; The macro form takes an s-expression re-writing program.
; Macros are expanded not applied.

((macro (λ exp (cons '- (cdr exp)))) + 1 2) ; error

; Macros may expand to macros.

b ; error: env-lookup
(set! a (macro (λ exp (list 'set! 'b (car exp))))) ; nil
(a 5) ; nil
b ; 5

; Expand expands a form where the left hand side (lhs) is a macro.

(expand '(a 5)) ; (set! b 5)

; define, lambda, let, and, or, cond, begin, when, and list are all macros.

; Mutation
; --------

; set! is the primitive environment editor.
; set! creates a new entry at the top-level environment if the variable is not otherwise located.

(set! a nil) ; nil
a ; nil
(set! b (λ _ a)) ; nil
(b) ; nil
(set! a 'a) ; nil
(b) ; a

; Conditionals
; ------------

; if then else is the primitive conditional.
; The only false value is #f, all other values are #t.

(if #t 'a 'b) ; a
(if #f (print 'a) (print 'b)) ; b nil
(if 'false 'true 'false) ; true

; if requires both true and false branches, see when for an alternate.

(if #t 'true) ; error

; Evaluation
; ----------

(eval 1) ; 1
(eval (eval 1)) ; 1

(1) ; error

; Variadic Expressions
; --------------------

; Macros can implement variable argument functions.

list ; macro
(list) ; nil
(list 1 2 3) ; (1 2 3)

; The standard macroS also define the associated re-writer.

list-rw ; (λ exp ...)
(list-rw (cdr '(list))) ; nil
(list-rw (cdr '(list 1 2 3))) ; (cons 1 (cons 2 (cons 3 nil)))

; Scheme
; ------

(map (+ 1) (list 1 2 3)) ; (2 3 4)
(map (compose (+ 1) (* 2)) (list 1 2 3)) ; (3 5 7)
(map (compose (/ 2) (+ 3)) (list 1 2 3)) ; (1/2 2/5 1/3)
(map (const 3) (list 1 2 3)) ; (3 3 3)
(cons (- 1 2) ((flip -) 1 2)) ; (cons -1 1)
(id 1) ; 1

(procedure? +) ; #t

; There is a macro, lambda, that approximates the scheme form.

(lambda-rw (cdr '(lambda () x))) ; (λ _ x)
(lambda-rw (cdr '(lambda (x) x))) ; (λ x x)
(lambda-rw (cdr '(lambda (x y) (cons x y)))) ; (λ x (λ y (cons x y)))
(lambda-rw (cdr '(lambda (x y z) (list x y z)))) ; (λ x (λ y (λ z (list x y z))))

((lambda () 1) nil) ; 1
((lambda (n) (* n n)) 3) ; 9
((lambda (x y z) (+ x (+ y ((lambda (n) (* n n)) z)))) 1 2 3) ; 12

; begin cannot be elided.

((lambda (p q) (display p) (print q))) ; error
((lambda (p q) (begin (display p) (print q))) 1 2) ; 12

; Nil
; ---

nil ; nil
(null? nil) ; #t

; Eq
; --

(equal? 'a 'a) ; #t
(equal? "b" "b") ; #t
(= 5 5) ; #t

; Ord
; ---

(< 0 1) ; #t
(> 0 1) ; #f
(min 1 2) ; 1
(max 1 2) ; 2
(compare 1 2) ; 'lt
(compare 2 1) ; 'gt
(compare 1 1) ; 'eq

; Begin
; -----

(begin-rw (cdr '(begin))) ; nil
(begin-rw (cdr '(begin (print 1)))) ; ((λ _ (print 1)) nil)
(begin-rw (cdr '(begin (print 1) (print 2)))) ; ((λ _ (print 2)) ((λ _ (print 1)) nil))

(begin (print 1) (print 2) (print 3)) ; prints 1 2 3 ; result=nil
((λ _ (print 3)) ((λ _ (print 2)) ((λ _ (print 1)) nil))) ; prints 1 2 3 ; result=nil

((λ x (begin (display x) (set! x 5) (print x))) 0) ; prints 05

; Define
; ------

(define-rw (cdr '(define one 1))) ; (set! one 1)
(define one 1) ; nil
one ; 1

(define sq (λ n ((* n) n))) ; nil
(sq 5) ; 25

(define sum-sq (lambda (p q) (+ (sq p) (sq q)))) ; nil
(sum-sq 7 9) ; 130

not-defined ; error
((lambda (_) (define not-defined 1)) nil) ; nil
not-defined ; 1

; Let Binding
; -----------

(let-rw (cdr '(let () 1))) ; 1
(let-rw (cdr '(let ((a 5)) (+ a 1)))) ; ((λ a (+ a 1)) 5)
(let-rw (cdr '(let ((a 5) (b 6)) (+ a b)))) ; ((λ a ((λ b (+ a b)) 6)) 5)

(let ((a 5)) a) ; 5
(let ((a 5) (b 6)) (cons a b)) ; (cons 5 6)
(let ((a 5) (b (+ 2 3))) (* a b)) ; 25
(let ((a 5) (b (+ a 3))) (* a b)) ; 40

(let ((set! 0) (set! 1)) set!) ; 1
set! ; error

; Let is unary

(let ((a 1)) (display a) (newline)) ; error
(let ((a 1)) (begin (display a) (newline))) ; 1\n

; Let is schemes let*.

(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))) ; 70 (not 35)

(letrec-rw (cdr '(letrec ((a 5) (b 6)) (cons a b))))
; (let ((a nil) (b nil)) (begin (set! a 5) (set! b 6) (cons a b)))

(define add-count
  (lambda (l)
    (letrec ((f (lambda (n l) (if (null? l) '() (cons (cons n (car l)) (f (+ n 1) (cdr l)))))))
      (f 0 l))))

(add-count (list 'a 'b 'c)) ; ((cons 0 a) (cons 1 b) (cons 2 c))

; Logic
; -----

(not #t) ; #f
(not #f) ; #t
(not 'sym) ; 0 ; #f

(and-rw (cdr '(and p q))) ; (if p q 0)
(list (and #t #t) (and #t #f) (and #f #t) (and #f #f)) ; (#t #f #f #f)

(or-rw (cdr '(or p q))) ; (if p #t q)
(list (or #t #t) (or #t #f) (or #f #t) (or #f #f)) ; (#t #t #t #f)

(cond-rw (cdr '(cond))) ; nil
(cond-rw (cdr '(cond (a b)))) ; (if a b nil)
(cond-rw (cdr '(cond (a b) (c d)))) ; (if a b (if c d nil))
(cond-rw (cdr '(cond (a b) (c d) (else e)))) ; (if a b (if c d e))
(cond-rw (cdr '(cond ((> x y) 'gt) ((< x y) 'lt) (else 'eq))))

(when-rw (cdr '(when a b))) ; (if a b nil)
(when #t (print 'true)) ; prints true
(when #f (print 'false)) ; nil

(when ((lambda (_) #t) nil) (print 'true)) ; prints true
(when ((lambda (_) #f) nil) (print 'false)) ; nil

; Mathematics
; -----------

; Binary operator UGens are optimising.

(Add 1 2) ; 3

; Symbolic aliases are given.

(+ 1 2) ; 3

; Constants are numbers.

(number? 1) ; #t
(number? 'one) ; #f
(number? (SinOsc kr 5 0)) ; #f

; Random
; ------

(s:rand 0 1) ; random floating point number in (0,1)
(s:irand 0 3) ; random integer in (0,2)

; Time
; ----

(begin (print 'before) (thread-sleep 1) (print 'after))

(utcr) ; <real>

(let ((t (utcr)))
  (begin
    (print 'before)
    (pause-thread-until (+ t 1))
    (print 'after)))

(define random-sine (Mul (SinOsc ar (Rand 220 440) 9) 0.01))
(dt-rescheduler (lambda (t) (begin (audition (Out 0 random-sine)) 1)) (utcr))

; IO
; --

newline-char ; 10
(write-char newline-char)
(newline) ; \n
(print 1) ; 1
(print (+ 1 2)) ; 3
(begin (display 1) (print 2)) ; 12
(define three (begin (display* 1) (print 2) 3)) ; 1 2 nil
three ; 3

; Strings
; -------

"string" ; "string"
(string? "string") ; #t

; Load
; ----

(load "/home/rohan/sw/hsc3-lisp/lisp/stdlib.lisp")

; Floating Point
; --------------

(map Sin (enum-from-then-to 0 0.05 pi))

; SICP
; ----

(define square (lambda (n) (* n n))) ; nil

(define f
  (lambda (x y)
    ((lambda (a b) (+ (+ (* x (square a)) (* y b)) (* a b)))
     (+ 1 (* x y))
     (- 1 y))))

(f 7 9) ; 28088

; UGen
; ----

(reset nil)
(reset)
(draw (Mul (SinOsc ar 440 0) 0.1))
(draw (Mul (SinOsc ar (MouseX kr 440 880 0 0.1) 0) 0.1))
(draw (Mul (HPZ1 (WhiteNoise ar)) 0.1))
(display-server-status nil)
(audition (Out 0 (Mul (SinOsc ar 440 0) 0.1)))

; Case Sensitivity
; ----------------

(audition (Out 0 (Mul (SinOsc ar 440 0) 0.1)))

; UID
; ---

(set! uid 0) ; nil
(map incr-uid '(1 1 1)) ; (1 2 3)
(unique-uid) ; 4

; Concurrency
; -----------

(begin
  (fork (begin (print 'a) (thread-sleep 4) (print 'c)))
  (thread-sleep 2)
  (print' b))

; After a thread is begun, it runs until it completes.

; Gensym
; ------

(list (gensym) (gensym))

; Derived cons
; ------------

; Cons need not be primitive, it can be in terms of λ.

(define kons (λ x (λ y (λ m (m x y)))))
(define kar (λ z (z (λ p (λ q p)))))
(define kdr (λ z (z (λ p (λ q q)))))

(kons 1 2) ; (λ m (m x y))
(kar (kons 1 2)) ; 1
(kdr (kons 1 2)) ; 2

; Y
; -

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

; Interpreter
; -----------

(env-print)
