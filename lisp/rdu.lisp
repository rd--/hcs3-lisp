(define exp-rand-n
  (lambda (nc lo hi)
    (mk-ugen (list "ExpRandN" ir (list lo hi) nil nc nil (incr-uid 1)))))

(define lin-rand-n
  (lambda (nc lo hi)
    (mk-ugen (list "LinRandN" ir (list lo hi) nil nc nil (incr-uid 1)))))

(define rand-n
  (lambda (nc lo hi)
    (mk-ugen (list "RandN" ir (list lo hi) nil nc nil (incr-uid 1)))))
