; Control.Monad

(define replicate-m-rw
  (λ exp
    (list (quote replicate-m*) (car exp) (list (quote λ) (quote _) (cadr exp)))))

(define replicate-m (macro replicate-m-rw))
