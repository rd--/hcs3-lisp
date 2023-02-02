; rate
(define dr (quote dr))
(define ir (quote ir))
(define kr (quote kr))
(define ar (quote ar))

(define demand-rate (quote dr))
(define scalar-rate (quote ir))
(define control-rate (quote kr))
(define audio-rate (quote ar))

; (vector 1 2)
(define vector-rw (λ exp (cons 'make-mce (cons (cons 'list exp) nil))))
(define vector (macro vector-rw))

; rand
(define random-float-uniform (lambda (l r) (unrand (Rand l r))))
(define random-int-uniform (lambda (l r) (unrand (IRand l (- r 1))))) ; i-rand is INCLUSIVE

; ugen
(define construct-ugen (lambda (p1 p2 p3 p4 p5 p6) (mk-ugen (list p1 p2 p3 p4 p5 p6))))

; osc
(define message (lambda (addr param) (cons addr param)))
