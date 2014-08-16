; uid
(define uid 0)
(define incr-uid (λ n (begin (set! uid (+ uid n)) uid)))
(define clone (lambda (n u) (clone* (list (incr-uid 1) n u))))

; mrg
(define make-mrg (lambda (p q) (make-mrg* (list p q))))

; scheme_rename_def
(define abs u:abs)
(define cos u:cos)
(define exp u:exp)
(define floor u:floor)
(define log u:log)
(define not u:not)
(define sin u:sin)
(define sqrt u:sqrt)
(define tan u:tan)
(define gcd u:gcd)
(define lcm u:lcm)
(define max u:max)
(define min u:min)
(define mod u:mod)
(define round u:round)

; operator_sym_def
(define + add)
(define - sub)
(define * mul)
(define / f-div)
(define % mod)
(define == eq-)
(define /= ne)
(define < lt-)
(define > gt-)
(define <= le)
(define >= ge)
(define ** pow)

; ord
(define eq eq-)
(define lt lt-)
(define gt gt-)

(define play-at (lambda (fd u nid act grp) (play-at* (list fd u nid act grp))))
(define audition (lambda (u) (play-at nil u -1 add-to-head 1)))
(define reset reset*)
(define async (lambda (_ msg) (async* msg)))
(define send (lambda (_ msg) (send* msg)))
(define with-sc3 (lambda (f) (f nil)))

; rand
(define random (lambda (l r) (unrand (rand l r))))
(define i-random (lambda (l r) (unrand (i-rand l r))))

; dot
(define draw show-graph)

; math (hsc3 constants are ieee double precision, scsynth is single precision)
(define inf 9e8)
(define -inf -9e8)

; scheme
(define expt pow)
