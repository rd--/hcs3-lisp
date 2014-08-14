; Control.Monad

(define replicate-m-rw (λ exp (list 'replicate-m* (car exp) (list 'λ '_ (cadr exp)))))
(define replicate-m (macro replicate-m-rw))
