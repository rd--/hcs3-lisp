(define ceiling Ceil)
(define constant (lambda (n) n))
(define klankSpec klankDataMce)
;(define mce asMce)
(define mrg mrg-n)
(define negate Neg)
(define uclone clone)

(define div /)

(define list-ref !!)

(define dup
  (lambda (f k)
    (if (== k 0) nil (cons (f) (dup f (- k 1))))))
(define ! dup)
(define !+ (lambda (f k) (sum (! f k))))

(define mixFill (lambda (k f) (!+ f k)))

;(define Dup dup)
