; rate
(define dr (quote dr))
(define ir (quote ir))
(define kr (quote kr))
(define ar (quote ar))

; ugen
(define construct-ugen (lambda (p1 p2 p3 p4 p5 p6) (mk-ugen (list p1 p2 p3 p4 p5 p6))))

; osc
(define message (lambda (addr param) (cons addr param)))
