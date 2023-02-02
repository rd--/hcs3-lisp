; define is an alias for set!

(set! define-rw (λ exp (cons (quote set!) exp)))
(set! define (macro define-rw))

; c....r

(define caar (λ c (car (car c))))
(define cadr (λ c (car (cdr c))))
(define cdar (λ c (cdr (car c))))
(define cddr (λ c (cdr (cdr c))))
(define caaar (λ c (car (car (car c)))))
(define caadr (λ c (car (car (cdr c)))))
(define cadar (λ c (car (cdr (car c)))))
(define caddr (λ c (car (cdr (cdr c)))))
(define cdaar (λ c (cdr (car (car c)))))
(define cdadr (λ c (cdr (car (cdr c)))))
(define cddar (λ c (cdr (cdr (car c)))))
(define cdddr (λ c (cdr (cdr (cdr c)))))
(define caaaar (λ c (car (car (car (car c))))))
(define caaadr (λ c (car (car (car (cdr c))))))
(define caadar (λ c (car (car (cdr (car c))))))
(define caaddr (λ c (car (car (cdr (cdr c))))))
(define cadaar (λ c (car (cdr (car (car c))))))
(define cadadr (λ c (car (cdr (car (cdr c))))))
(define caddar (λ c (car (cdr (cdr (car c))))))
(define cadddr (λ c (car (cdr (cdr (cdr c))))))
(define cdaaar (λ c (cdr (car (car (car c))))))
(define cdaadr (λ c (cdr (car (car (cdr c))))))
(define cdadar (λ c (cdr (car (cdr (car c))))))
(define cdaddr (λ c (cdr (car (cdr (cdr c))))))
(define cddaar (λ c (cdr (cdr (car (car c))))))
(define cddadr (λ c (cdr (cdr (car (cdr c))))))
(define cdddar (λ c (cdr (cdr (cdr (car c))))))
(define cddddr (λ c (cdr (cdr (cdr (cdr c))))))

; nil

(define nil '())

; lambda

(define lambda-rw-code
  (quote
   (λ exp
     (let ((param (car exp))
           (code (cadr exp))
           (rem (cddr exp)))
       (cond ((pair? rem) (error "lambda: not unary?"))
             ((null? param) (list (quote λ) (quote _) code))
             ((null? (cdr param)) (list (quote λ) (car param) code))
             (else (list (quote λ) (car param) (lambda-rw (list (cdr param) code)))))))))

; (expand lambda-rw-code)
;(define lambda-rw (λ exp ((λ param ((λ code (if (pair? (cddr exp)) (error "lambda: not unary?") (if (null? param) (cons (quote λ) (cons (quote _) (cons code nil))) (if (null? (cdr param)) (cons (quote λ) (cons (car param) (cons code nil))) (cons (quote λ) (cons (car param) (cons (lambda-rw (cons (cdr param) (cons code nil))) nil))))))) (cadr exp))) (car exp))))
(define lambda-rw (λ exp ((λ param ((λ code ((λ rem (if (pair? rem) (error "lambda: not unary?") (if (null? param) (cons (quote λ) (cons (quote _) (cons code nil))) (if (null? (cdr param)) (cons (quote λ) (cons (car param) (cons code nil))) (cons (quote λ) (cons (car param) (cons (lambda-rw (cons (cdr param) (cons code nil))) nil))))))) (cddr exp))) (cadr exp))) (car exp))))

(define lambda (macro lambda-rw))

; gensym

(define gensym-code
  (quote
   (let ((n 0))
     (lambda ()
       (let ((r (string->symbol (string-append "gensym:" (show n)))))
         (begin
           (set! n (+ n 1))
           r))))))

; (expand gensym-code)
(define gensym ((λ n (λ _ ((λ r ((λ _ r) ((λ _ (set! n (+ n 1))) nil))) (string->symbol (string-append "gensym:" (show n)))))) 0))

; let

(define let-rw-code
  (quote
   (λ exp
     (let ((bindings (car exp))
           (code (cadr exp))
           (rem (cddr exp)))
       (cond ((pair? rem) (error "let: not unary?"))
             ((null? bindings) code)
             (else (let ((binding0 (car bindings)))
                     (list (list (quote λ) (car binding0) (let-rw (list (cdr bindings) code)))
                           (cadr binding0)))))))))

; (expand let-rw-code)
(define let-rw (λ exp ((λ bindings ((λ code ((λ rem (if (pair? rem) (error "let: not unary?") (if (null? bindings) code ((λ binding0 (cons (cons (quote λ) (cons (car binding0) (cons (let-rw (cons (cdr bindings) (cons code nil))) nil))) (cons (cadr binding0) nil))) (car bindings))))) (cddr exp))) (cadr exp))) (car exp))))

(define let (macro let-rw))

; list

(define append
  (lambda (a b)
    (if (null? a)
        b
        (cons (car a) (append (cdr a) b)))))

; foldr :: (a -> b -> b) -> b -> [a] -> b
(define foldr
  (lambda (f z l)
    (if (null? l)
        z
        (f (car l) (foldr f z (cdr l))))))

; (list-rw '(list 1 2))
(define list-rw
  (λ exp
     (let ((f (lambda (e r) (append (cons (quote cons) (cons e nil)) (cons r '())))))
       (foldr f (quote nil) exp))))

(define list (macro list-rw))

; and / or

(define and-rw (λ exp (if (pair? (cddr exp)) (error "and: not binary?") (list (quote if) (car exp) (cadr exp) #f))))
(define and (macro and-rw))

(define or-rw (λ exp (if (pair? (cddr exp)) (error "or: not binary?") (list (quote if) (car exp) #t (cadr exp)))))
(define or (macro or-rw))

; cond

(define cond-rw
  (λ exp
     (if (null? exp)
         nil
         (let ((c0 (car exp)))
           (if (equal? (car c0) (quote else))
               (cadr c0)
               (list (quote if) (car c0) (cadr c0) (cond-rw (cdr exp))))))))

(define cond (macro cond-rw))

(define when-rw
  (λ exp
    (let ((test (car exp))
          (branch (cadr exp)))
      (list (quote if) test branch nil))))

(define when (macro when-rw))

; begin

(define begin-rw*-code
  (quote
   (lambda (pre code)
     (if (null? code)
         pre
         (begin-rw* (list (list (quote λ) (quote _) (car code)) pre) (cdr code))))))

; (expand begin-rw*-code)
(define begin-rw* (λ pre (λ code (if (null? code) pre (begin-rw* (cons (cons (quote λ) (cons (quote _) (cons (car code) nil))) (cons pre nil)) (cdr code))))))

(define begin-rw (λ exp (begin-rw* nil exp)))

(define begin (macro begin-rw))

; letrec

(define letrec-rw-code
  (quote
   (λ exp
     (let ((bindings (car exp))
           (code (cadr exp))
           (rem (cddr exp)))
       (cond ((pair? rem) (error "letrec: not unary?"))
             ((null? bindings) code)
             (else (let ((names (map car bindings))
                         (values (map cadr bindings))
                         (bindings* (map (λ x (list x (quote undefined))) names))
                         (initialisers (zipWith (lambda (p q) (list (quote set!) p q)) names values)))
                     (list (quote let)
                           bindings*
                           (cons (quote begin) (append initialisers (list code)))))))))))

; (expand letrec-rw-code)
(define letrec-rw (λ exp ((λ bindings ((λ code ((λ rem (if (pair? rem) (error "letrec: not unary?") (if (null? bindings) code ((λ names ((λ values ((λ bindings* ((λ initialisers (cons (quote let) (cons bindings* (cons (cons (quote begin) (append initialisers (cons code nil))) nil)))) (zipWith (λ p (λ q (cons (quote set!) (cons p (cons q nil))))) names values))) (map (λ x (cons x (cons (quote undefined) nil))) names))) (map cadr bindings))) (map car bindings))))) (cddr exp))) (cadr exp))) (car exp))))

(define letrec (macro letrec-rw))

; EXPAND

; simple fixed expander, pre-dates Lisp.hs:expand

;(define expand*
;  (λ exp
;    (if (list? exp)
;        (let ((f (λ nm (equal? (car exp) nm))))
;          (cond ((f 'list) (expand (list-rw (map expand (cdr exp)))))
;                ((f 'let) (expand (let-rw (map expand (cdr exp)))))
;                ((f 'cond) (expand (cond-rw (map expand (cdr exp)))))
;                ((f 'lambda) (expand (lambda-rw (map expand (cdr exp)))))
;                ((f 'begin) (expand (begin-rw (map expand (cdr exp)))))
;                (else (map expand exp))))
;        exp)))

; MATH

(define pi 3.141592653589793)

; IO

(define newline-char 10)
(define newline (λ _ (write-char newline-char)))
(define display (λ s (write-string (show s)))) ; prints string quotes...

(define print (λ o (begin (display o) (newline))))

(define space-char 32)
(define space (λ _ (write-char space-char)))

(define display* (λ o (begin (display o) (space))))

(define for-each
  (lambda (f l)
    (if (null? l)
        nil
        (begin
          (f (car l))
          (for-each f (cdr l))))))

; NO DEFINE-SYNTAX!

(define define-syntax
  (macro
      (lambda (exp)
        (begin (print "DEFINE-SYNTAX DISCARDED") (print exp) (quote define-syntax)))))

; NO IMPORT!

(define import
  (macro
      (lambda (exp)
        (begin (print "IMPORT DISCARDED") (print exp) (quote import)))))
