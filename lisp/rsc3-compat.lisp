;; COMPAT

(define make-mrg (lambda (p q) (make-mrg* (list p q))))

;; scheme_rename_def
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

;; operator_sym_def
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

(define eq eq-)
(define lt lt-)
(define gt gt-)

(define play-at
  (lambda (fd u nid act grp)
    (play-at* (list fd u nid act grp))))

(define reset (lambda (_) (reset*)))

(define audition (lambda (u) (play-at nil u -1 add-to-head 1)))

(define with-sc3 (lambda (f) (f nil)))
