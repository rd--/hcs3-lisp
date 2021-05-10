; uid
(define uid 0)
(define incr-uid (λ n (begin (set! uid (+ uid n)) uid)))
(define unique-uid (lambda () (incr-uid 1)))

; clone/mce
(define clone (lambda (n f) (make-mce (replicate-m n f))))

; mrg
(define make-mrg (lambda (p q) (make-mrg* (list p q)))) ; make-mrg* is primitive

; [ugen] -> mrg
(define mrg-n
  (lambda (xs)
    (if (null? xs)
       (error "mrg-n" "nil input list" xs)
       (if (null? (tail xs))
           (head xs)
           (mrg2 (head xs) (mrg-n (tail xs)))))))

; scheme_rename_def
;(define abs Abs)
;(define cos Cos)
;(define exp Exp)
;(define floor Floor)
;(define log Log)
;(define not Not)
;(define sin Sin)
;(define sqrt Sqrt)
;(define tan Tan)
;(define gcd GCD)
;(define lcm LCM)
;(define max Max)
;(define min Min)
;(define mod Mod)
;(define round Round)

; operator_sym_def
(define + Add)
(define - Sub)
(define * Mul)
(define / FDiv)
(define % Mod)
(define == EQ)
(define /= NE)
(define < LT)
(define > GT)
(define <= LE)
(define >= GE)
(define ** Pow)

; ord
;(define eq EQ)
;(define lt LT)
;(define gt GT)

(define play-at (lambda (fd u nid act grp) (play-at* (list fd u nid act grp))))
(define audition (lambda (u) (play-at nil u -1 add-to-head 1)))
(define reset reset*)
(define async (lambda (_ msg) (async* msg)))
(define send (lambda (_ msg) (send* msg)))
(define with-sc3 (lambda (f) (f nil)))

; rand
(define s:rand (lambda (l r) (unrand (Rand l r))))
(define s:irand (lambda (l r) (unrand (IRand l (- r 1))))) ; i-rand is INCLUSIVE

; dot
(define draw show-graph)

; math (hsc3 constants are ieee double precision, scsynth is single precision)
(define inf 9e8)
(define -inf -9e8)

; hsc3
(define useq (lambda (n f x) ((foldl1 compose (replicate n f)) x)))

; scheme
(define expt Pow)

; control
(define ctl (lambda (rt nm df) (mk-ctl (list rt nm df)))) ; mk-ctl is primitive
