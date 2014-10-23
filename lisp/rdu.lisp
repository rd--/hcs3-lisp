(define rand-n
  (lambda (nc lo hi)
    (mk-ugen (list "RandN" ir (list lo hi) nil nc nil (incr-uid 1)))))
