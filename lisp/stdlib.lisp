; DEFINE

(set! define-rw (λ exp (cons 'set! exp)))
(set! define (macro define-rw))

; NIL

(define nil '())

; LAMBDA

(define lambda-rw-code
  (quote
   (λ exp
     (let ((param (car exp))
           (code (car (cdr exp))))
       (cond ((null? param) (list 'λ '_ code))
             ((null? (cdr param)) (list 'λ (car param) code))
             (else (list 'λ (car param) (lambda-rw (list (cdr param) code)))))))))

; (expand lambda-rw-code)

(define lambda-rw (λ exp ((λ param ((λ code (if (null? param) (cons (quote λ) (cons (quote _) (cons code nil))) (if (null? (cdr param)) (cons (quote λ) (cons (car param) (cons code nil))) (cons (quote λ) (cons (car param) (cons (lambda-rw (cons (cdr param) (cons code nil))) nil)))))) (car (cdr exp)))) (car exp))))

(define lambda (macro lambda-rw))

; LET

(define let-rw-code
  (quote
   (λ exp
     (if (null? (car exp))
         (cadr exp)
         (list (list 'λ (caaar exp) (let-rw (list (cdar exp) (cadr exp))))
               (cadr (caar exp)))))))

; (expand let-rw-code)

(define let-rw (λ exp (if (null? (car exp)) (cadr exp) (cons (cons (quote λ) (cons (caaar exp) (cons (let-rw (cons (cdar exp) (cons (cadr exp) nil))) nil))) (cons (cadr (caar exp)) nil)))))

(define let (macro let-rw))

; LIST

(define append
  (lambda (a b)
    (if (null? a)
        b
        (cons (car a) (append (cdr a) b)))))

;; foldr :: (a -> b -> b) -> b -> [a] -> b
(define foldr
  (lambda (f z l)
    (if (null? l)
        z
        (f (car l) (foldr f z (cdr l))))))

(define list-rw
  (λ exp
    (let ((f (lambda (e r) (append (cons 'cons (cons e nil)) (cons r '())))))
      (foldr f 'nil exp))))

(define list (macro list-rw))

; AND / OR

(define and-rw (λ exp (list 'if (car exp) (cadr exp) #f)))
(define and (macro and-rw))

(define or-rw (λ exp (list 'if (car exp) #t (cadr exp))))
(define or (macro or-rw))

; COND

(define cond-rw
  (λ exp
     (if (null? exp)
         nil
         (let ((c0 (car exp)))
           (if (equal? (car c0) 'else)
               (cadr c0)
               (list 'if (car c0) (cadr c0) (cond-rw (cdr exp))))))))

(define cond (macro cond-rw))

(define when-rw
  (λ exp
    (let ((test (car exp))
          (branch (cadr exp)))
      (list 'if test branch nil))))

(define when (macro when-rw))

; BEGIN

(define begin-rw*-code
  (quote
   (lambda (pre code)
     (if (null? code)
         pre
         (begin-rw* (list (list 'λ '_ (car code)) pre) (cdr code))))))

; (expand begin-rw*-code)

(define begin-rw* (λ pre (λ code (if (null? code) pre (begin-rw* (cons (cons (quote λ) (cons (quote _) (cons (car code) nil))) (cons pre nil)) (cdr code))))))

(define begin-rw (λ exp (begin-rw* nil exp)))

(define begin (macro begin-rw))

; LETREC

(define letrec-rw-code
  (quote
   (lambda (exp)
     (let ((bindings (car exp))
           (code (cadr exp))
           (names (map car bindings))
           (values (map cadr bindings))
           (bindings* (zip-with list names (map (const nil) bindings)))
           (initialisers (zip-with (lambda (p q) (list 'set! p q)) names values)))
       (list 'let bindings* (cons 'begin (append initialisers (list code))))))))

; (expand letrec-rw-code)

(define letrec-rw (λ exp ((λ bindings ((λ code ((λ names ((λ values ((λ bindings* ((λ initialisers (cons (quote let) (cons bindings* (cons (cons (quote begin) (append initialisers (cons code nil))) nil)))) (zip-with (λ p (λ q (cons (quote set!) (cons p (cons q nil))))) names values))) (zip-with list names (map (const nil) bindings)))) (map cadr bindings))) (map car bindings))) (cadr exp))) (car exp))))

(define letrec (macro letrec-rw))

; EXPAND

; simple fixed expander, pre-dates Lisp.hs:expand

(define expand'
  (λ exp
    (if (list? exp)
        (let ((f (λ nm (equal? (car exp) nm))))
          (cond ((f 'list) (expand (list-rw (map expand (cdr exp)))))
                ((f 'let) (expand (let-rw (map expand (cdr exp)))))
                ((f 'cond) (expand (cond-rw (map expand (cdr exp)))))
                ((f 'lambda) (expand (lambda-rw (map expand (cdr exp)))))
                ((f 'begin) (expand (begin-rw (map expand (cdr exp)))))
                (else (map expand exp))))
        exp)))

; MATH

(define pi 3.141592653589793)

; IO

(define newline-char 10)
(define newline (λ _ (write-char newline-char)))

(define print (λ o (begin (display o) (newline))))

(define space-char 32)
(define space (λ _ (write-char space-char)))

(define display* (λ o (begin (display o) (space))))

; C....R

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

; NO DEFINE-SYNTAX!

(define define-syntax
  (macro
      (lambda (exp)
        (begin (print "DEFINE-SYNTAX DISCARDED") (print exp) 'define-syntax))))

; NO IMPORT!

(define import
  (macro
      (lambda (exp)
        (begin (print "IMPORT DISCARDED") (print exp) 'import))))

; RHS

(define replicate-m-rw (lambda (exp) (list 'replicate-m* (car exp) (list 'lambda '(_) (cadr exp)))))
(define replicate-m (macro replicate-m-rw))
