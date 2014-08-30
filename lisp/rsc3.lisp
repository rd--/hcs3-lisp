;; scsynth commands

(define b-alloc (lambda (b nf nc) (message "/b_alloc" (list b nf nc))))
(define b-alloc-read (lambda (b fn f0 n) (message "/b_allocRead" (list b fn f0 n))))
(define b-alloc-read-channel (lambda (nid p f n cs) (message "/b_allocReadChannel" (append (list nid p f n) cs))))
(define b-close (lambda (i) (message "/b_close" (list i))))
(define b-fill1 (lambda (i j k f) (message "/b_fill" (list i j k f))))
(define b-free (lambda (i) (message "/b_free" (list i))))
(define b-gen1 (lambda (i s fs) (message "/b_gen" (cons i (cons s fs)))))
(define b-get1 (lambda (i j) (message "/b_get" (list i j))))
(define b-getn1 (lambda (i j k) (message "/b_getn" (list i j k))))
(define b-query1 (lambda (i) (message "/b_query" (list i))))
(define b-read (lambda (id path frame n bframe flag) (message "/b_read" (list id path frame n bframe flag))))
(define b-set1 (lambda (i j f) (message "/b_set" (list i j f))))
(define b-setn1 (lambda (i j fs) (message "/b_setn" (cons i (cons j (cons (length fs) fs))))))
(define b-write (lambda (id path header type frames start flag) (message "/b_write" (list id path header type frames start flag))))
(define b-zero (lambda (i) (message "/b_zero" (list i))))

(define c-fill1 (lambda (i j f) (message "/c_fill" (list i j f))))
(define c-get1 (lambda (i) (message "/c_get" (list i))))
(define c-getn1 (lambda (i j) (message "/c_getn" (list i j))))
(define c-set1 (lambda (i f) (message "/c_set" (list i f))))
(define c-setn1 (lambda (i fs) (message "/c_setn" (cons i (cons (length fs) fs)))))

(define d-free1 (lambda (s) (message "/d_free" (list s))))
(define d-load (lambda (s) (message "/d_load" (list s))))
(define d-load-dir (lambda (s) (message "/d_loadDir" (list s))))
(define d-recv (lambda (b) (message "/d_recv" (list b))))

(define g-deep-free1 (lambda (i) (message "/g_deepFree" (list i))))
(define g-free-all1 (lambda (i) (message "/g_freeAll" (list i))))
(define g-head1 (lambda (i j) (message "/g_head" (list i j))))
(define g-new1 (lambda (i j k) (message "/g_new" (list i j k))))
(define g-tail1 (lambda (i j) (message "/g_tail" (list i j))))

(define n-before (lambda (i j) (message "/n_before" (list i j))))
(define n-fill1 (lambda (i s j f) (message "/n_fill" (list i s j f))))
(define n-free1 (lambda (i) (message "/n_free" (list i))))
(define n-map (lambda (i s j) (message "/n_map" (list i s j))))
(define n-mapn1 (lambda (i s j k) (message "/n_mapn" (list i s j k))))
(define n-query (lambda (i) (message "/n_query" (list i))))
(define n-run1 (lambda (i j) (message "/n_run" (list i j))))
(define n-set (lambda (i xys) (let ((z (concat-map (lambda (xy) (list (car xy) (cdr xy))) xys))) (message "/n_set" (cons i z)))))
(define n-set1 (lambda (i s f) (message "/n_set" (list i s f))))
(define n-setn1 (lambda (i s fs) (message "/n_setn" (cons i (cons s (cons (length fs) fs))))))
(define n-trace (lambda (i) (message "/n_trace" (list i))))

(define s-get1 (lambda (i j) (message "/s_get" (list i j))))
(define s-getn1 (lambda (i s j) (message "/s_getn" (list i s j))))
(define s-new (lambda (s i j k cs) (message "/s_new" (append (list s i j k) cs))))
(define s-new0 (lambda (s i j k) (message "/s_new" (list s i j k))))
(define s-new1 (lambda (s i j k t f) (message "/s_new" (list s i j k t f))))
(define s-new2 (lambda (s i j k t1 f1 t2 f2) (message "/s_new" (list s i j k t1 f1 t2 f2))))
(define s-noid (lambda (i) (message "/s_noid" (list i))))

(define clear-sched (message "/clearSched" nil))
(define dump-osc (lambda (i) (message "/dumpOSC" (list i))))
(define notify (lambda (i) (message "/notify" (list i))))
(define quit (message "/quit" nil))
(define status (message "/status" nil))
(define sync (lambda (i) (message "/sync" (list i))))
;; MATH

(define dinf 9e8)
(define e 2.718281828459045) ;; (exp 1.0)
(define pi 3.141592653589793) ;; (* 4.0 (atan 1.0))
(define two-pi (* 2 pi))
(define half-pi (* 0.5 pi))

;; ENUMERATION

(define add-to-head 0)
(define add-to-tail 1)
(define add-before 2)
(define add-after 3)
(define add-replace 4)

(define gen-normalize 1)
(define gen-wavetable 2)
(define gen-clear 4)

(define do-nothing 0)
(define pause-synth 1)
(define remove-synth 2)
(define remove-group 14)

(define no-loop 0)
(define loop 1)

(define linear 0)
(define exponential 1)
(define add3 sum3)
(define add4 sum4)

(define as-local-buf
  (lambda (l)
    (let* ((b (local-buf (length l) 1))
           (s (set-buf* b 0 l)))
      (mrg2 b s))))

(define buf-rd-c (lambda (nc r b p l) (buf-rd nc r b p l 4)))
(define buf-rd-l (lambda (nc r b p l) (buf-rd nc r b p l 2)))
(define buf-rd-n (lambda (nc r b p l) (buf-rd nc r b p l 1)))

(define cps-midi cpsmidi)

;; ugen -> ugen -> ugen
(define dcons
  (lambda (x xs)
    (let ((i (dseq 1 (mce2 0 1)))
	  (a (dseq 1 (mce2 x xs))))
      (dswitch i a))))

;; ugen -> ugen -> ugen -> ugen -> ugen -> ugen
(define dyn-klank*
  (lambda (i fs fo ds l)
    (if (null? l)
        0
        (let ((f (list-ref l 0))
              (a (list-ref l 1))
              (d (list-ref l 2)))
          (add (mul (ringz i (mul-add f fs fo) (mul d ds)) a)
               (dyn-klank* i fs fo ds (drop 3 l)))))))

(define dyn-klank
  (lambda (i fs fo ds s)
    (dyn-klank* i fs fo ds (mce-channels s))))

(define fdiv f-div)

(define fft* (lambda (buf in) (fft buf in 0.5 0 1 0)))

;; ugen -> ugen -> ugen -> ugen
(define freq-shift*
  (lambda (i f p)
    (let ((o (sin-osc ar f (mce2 (add p (* 0.5 pi)) p)))
	  (h (hilbert i)))
      (mix (mul h o)))))

(define hear (lambda (u) (audition (out 0 u))))

(define ifft* (lambda (buf) (ifft buf 0 0)))

;; [ugen] -> [ugen] -> [ugen] -> ugen
(define klang-data
  (lambda (freqs amps phases)
    (make-mce
     (concat
      (zip-with3
       list3
       freqs amps phases)))))

;; [ugen] -> [ugen] -> [ugen] -> ugen
(define klank-data klang-data)

;; ugen -> ugen -> ugen -> ugen
(define klank-data-mce
  (lambda (f a p)
    (klank-data (mce-channels f) (mce-channels a) (mce-channels p))))

(define l-choose
  (lambda (l)
    (select (i-rand 0 (length l)) (make-mce l))))

(define lin-lin
  (lambda (in srclo srchi dstlo dsthi)
    (let* ((scale (fdiv (sub dsthi dstlo) (sub srchi srclo)))
           (offset (sub dstlo (mul scale srclo))))
      (mul-add in scale offset))))

(define mce2 (lambda (a b) (make-mce (list a b))))
(define mce3 (lambda (a b c) (make-mce (list a b c))))
(define mce4 (lambda (a b c d) (make-mce (list a b c d))))
(define mce5 (lambda (a b c d e) (make-mce (list5 a b c d e))))

;; mce -> int -> ugen
(define mce-channel
  (lambda (u n)
    (list-ref (mce-channels u) n)))

;; ([ugen] -> [ugen]) -> (mce -> mce)
(define mce-edit
  (lambda (f)
    (lambda (u)
      (make-mce (f (mce-channels u))))))

;; int -> (int -> ugen) -> mce
(define mce-fill
  (lambda (n f)
    (make-mce (map f (enum-from-to 0 (- n 1))))))

;; (ugen -> ugen) -> mce -> mce
(define mce-map (lambda (f u) (make-mce (map f (mce-channels u)))))

;; mce -> mce
(define mce-reverse (mce-edit reverse))

;; mce -> mce
(define mce-transpose
  (lambda (u)
    (make-mce
     (map make-mce (transpose (map mce-channels (mce-channels u)))))))

(define midi-cps midicps)

;; ugen|mce -> ugen
(define mix (lambda (u) (foldl add 0 (mce-channels u))))

;; int -> (int -> ugen) -> ugen
(define mix-fill (lambda (n f) (mix (mce-fill n f))))

;; Rate -> UGen -> UGen -> Warp -> UGen -> UGen
(define mouse-r
  (lambda (rt l r ty tm)
    (let ((f (if (= ty 0) lin-lin lin-exp)))
      (lag (f (lf-noise1 rt 1) -1 1 l r) tm))))

(define mouse-x* mouse-r)
(define mouse-y* mouse-r)

(define mouse-button*
  (lambda (rt l r tm)
    (let ((o (lf-clip-noise rt 1)))
      (lag (lin-lin o -1 1 l r) tm))))

(define mrg2 make-mrg)
(define mrg3 (lambda (a b c) (mrg2 a (mrg2 b c))))
(define mrg4 (lambda (a b c d) (mrg2 a (mrg3 b c d))))
(define mrg5 (lambda (a b c d e) (mrg2 a (mrg4 b c d e))))

;; [ugen] -> mrg
(define mrg-n
  (lambda (xs)
    (if (null? xs)
	(error "mrg-n" "nil input list" xs)
	(if (null? (tail xs))
	    (head xs)
	    (mrg2 (head xs) (mrg-n (tail xs)))))))

(define mul3 (lambda (a b c) (mul (mul a b) c)))
(define mul4 (lambda (a b c d) (mul (mul (mul a b) c) d)))

;; [m] -> [p] -> [#, m, p...]
(define packfft-data
  (lambda (m p)
    (make-mce
     (cons (* 2 (length m))
	   (concat (zip-with list m p))))))

;; [[m, p]] -> [#, m, p...]
(define packfft-data*
  (lambda (mp)
    (make-mce
     (cons (* 2 (length mp))
	   (concat mp)))))

;; double -> void
(define pause-thread thread-sleep)

;; double -> void
(define pause-thread-until
  (lambda (t)
    (let ((c (utcr)))
      (when (> t c) (pause-thread (- t c))))))

;; port -> ugen -> ()
(define play
  (lambda (fd u)
    (play-at fd u -1 add-to-tail 1)))

;; rate -> ugen -> ugen -> ugen -> ugen -> ugen
(define pm-osc
  (lambda (r cf mf pm mp)
    (sin-osc r cf (mul (sin-osc r mf mp) pm))))

(define pvcollect
  (lambda (c nf f from to z?)
    (let* ((m (unpack-fft c nf from to 0))
	   (p (unpack-fft c nf from to 1))
	   (i (enum-from-to from to))
	   (e (zip-with3 f m p i)))
      (pack-fft c nf from to z? (packfft-data* e)))))

(define rand* rand-)

(define rand2 (lambda (n) (rand (neg n) n)))

; the cardinality input is derived from the values input...
(define set-buf*
  (lambda (buf offset values)
    (set-buf buf offset (length values) (make-mce values))))

;; ugen -> ugen
(define sound-in
  (lambda (n)
    (if (mce? n)
	(let ((l (mce-channels n)))
	  (if (consecutive? l)
	      (in (length l) ar (add num-output-buses (head l)))
	      (in 1 ar (add num-output-buses n))))
	(in 1 ar (add num-output-buses n)))))

(define t-choose
  (lambda (trig array)
    (select (ti-rand 0 (length (mce-channels array)) trig) array)))

(define tw-choose
  (lambda (trig array weights normalize)
    (select (tw-index trig normalize weights) array)))

(define tw-index t-windex)

(define unpack-fft
  (lambda (c nf from to mp?)
    (map (lambda (i)
            (unpack1-fft c nf i mp?))
	 (enum-from-to from to))))

(define with-sc3*
  (lambda (l)
    (with-sc3 (lambda (fd) (map (lambda (f) (f fd)) l)))))

; RANDOM

;; float -> float -> float
(define exp-random
  (lambda (a b)
    (let ((r (/ b a)))
      (* (expt r (random 0 1)) a))))

;; [a] -> a
(define choose
  (lambda (xs)
    (list-ref xs (i-random 0 (length xs)))))
;; ENVELOPES

;; symbol|number -> number
(define curve-to-shape
  (lambda (c)
    (cond
     ((symbol? c)
      (cond ((equal? c 'step) 0.0)
	    ((equal? c 'linear) 1.0)
	    ((equal? c 'exponential) 2.0)
	    ((equal? c 'sine) 3.0)
	    ((equal? c 'welch) 4.0)
	    ((equal? c 'squared) 6.0)
	    ((equal? c 'cubed) 7.0)
	    (else (error "curve-to-shape" "symbol" c))))
     ((number? c)
      5.0)
     (else
      (error "curve-to-shape" "illegal curve" c)))))

;; any -> number
(define curve-to-value
  (lambda (c)
    (if (number? c) c 0.0)))

;; Make a <list> for use with the EnvGen UGen. `levels' is a <list>
;; containing the left to right gain values for the envelope, it has
;; one more element than the <list> `times', having the delta times
;; for each envelope segment. `curve' is either a string or a number
;; or a <list> of such, in either case it is expanded to a list of the
;; same length as `times'. `release-node' is the index of the
;; 'release' stage of the envelope, `loop-node' is the index of the
;; 'loop' stage of the envelope. These indices are set as invalid, by
;; convention -1, to indicate there is no such node.
(define env
  (lambda (levels times curves release-node loop-node)
    (make-mce
     (append
      (list (head levels) (length times) release-node loop-node)
      (concat
       (zip-with3
	(lambda (l t c)
	  (list l
		t
		(curve-to-shape c)
		(curve-to-value c)))
	(tail levels)
	times
	curves))))))

;; Co-ordinate based static envelope generator.
;; [(ugen . ugen)] -> ugen -> ugen -> [ugen] -> ugen
(define env-coord
  (lambda (d dur amp curves)
    (env (map (lambda (e) (mul (cdr e) amp)) d)
         (map (lambda (e) (mul e dur)) (d->dx (map car d)))
         curves
         -1
         -1)))

(define env-coord-linear
  (lambda (d dur amp)
    (env-coord d dur amp (replicate (- (length d) 1) 1))))

;; (equal? (mk-coord (list 1 2 3 4)) (list (cons 1 2) (cons 3 4)))
(define mk-coord
  (lambda (l)
    (if (null? l)
        '()
        (let ((x (car l))
              (y (cadr l))
              (r (cddr l)))
          (cons (cons x y) (mk-coord r))))))

(define env-bp
  (lambda (bp d a c) (env-coord (mk-coord bp) d a c)))

(define env-bp-linear
  (lambda (bp d a)
    (env-coord-linear (mk-coord bp) d a)))

;; Design a standard trapezoidal envelope. `shape' determines the
;; sustain time as a proportion of `dur', zero is a triangular
;; envelope, one a rectangular envelope. `skew' determines the
;; attack/decay ratio, zero is an immediate attack and a slow decay,
;; one a slow attack and an immediate decay. This implementation
;; builds a zero one breakpoint data set and calls env-coord.
(define env-trapezoid
  (lambda (shape skew dur amp)
    (let* ((x1 (mul skew (sub 1.0 shape)))
	   (bp (list (cons 0 (le skew 0.0))
		     (cons x1 1.0)
		     (cons (add shape x1) 1.0)
		     (cons 1.0 (ge skew 1.0)))))
      (env-coord bp dur amp (replicate 3 'linear)))))

(define env-triangle
  (lambda (dur level)
    (let ((half-dur (mul dur 0.5)))
      (env (list 0.0 level 0.0)
	   (list half-dur half-dur)
	   (list 'linear 'linear)
	   -1
	   -1))))

(define env-sine
  (lambda (dur level)
    (let ((half-dur (mul dur 0.5)))
      (env (list 0.0 level 0.0)
	   (list half-dur half-dur)
	   (list 'sine 'sine)
	   -1
	   -1))))

(define env-perc
  (lambda (attackTime releaseTime level curves)
    (env (list 0.0 level 0.0)
	 (list attackTime releaseTime)
	 curves
	 -1
	 -1)))

(define env-adsr
  (lambda (attackTime
	   decayTime
	   sustainLevel
	   releaseTime
	   peakLevel
	   curves
	   bias)
    (env (map (lambda (e) (mul e bias))
               (list 0.0 peakLevel (mul peakLevel sustainLevel) 0.0))
	 (list attackTime decayTime releaseTime)
	 curves
	 2
	 -1)))

(define env-asr
  (lambda (attackTime sustainLevel releaseTime curves)
    (env (list 0.0 sustainLevel 0.0)
	 (list attackTime releaseTime)
	 curves
	 1
	 -1)))

(define env-linen
  (lambda (attackTime sustainTime releaseTime level curves)
    (env (list 0.0 level level 0.0)
	 (list attackTime sustainTime releaseTime)
	 (if (null? curves) (list 'linear 'linear 'linear) curves)
	 -1
	 -1)))
;; [a] -> int -> [a]
(define extend
  (lambda (l n)
    (let ((z (length l)))
      (cond ((= z n) l)
	    ((> z n) (take n l))
	    (else (extend (append l l) n))))))

;; [a] -> int -> [a]
(define take-cycle
  (lambda (l n)
    (if (null? l)
	nil
	(cons (head l)
	      (take-cycle (drop n l) n)))))

;; (a -> a -> a) -> ([a] -> [a])
(define differentiate-with
  (lambda (f)
    (lambda (l)
      (zip-with f l (cons 0 l)))))

;; num a => [a] -> [a]
;;
;; (equal? (differentiate '(1 2 4 7 11)) '(1 1 2 3 4))
(define differentiate
  (differentiate-with -))

;; (a -> a -> a) -> ([a] -> [a])
(define integrate-with
  (lambda (f)
    (lambda (l)
      (let ((x (car l))
            (xs (cdr l))
            (g (lambda (a x) (let ((y (f a x))) (cons y y)))))
        (cons x (cdr (map-accum-l g x xs)))))))

;; num a => [a] -> [a]
;;
;; (equal? (integrate (list 3 4 1 1)) (list 3 7 8 9))
;; (equal? (integrate '(1 1 2 3 4)) '(1 2 4 7 11))
(define integrate
  (integrate-with +))

(define d->dx
  (lambda (l)
    (zip-with sub (drop 1 l) l)))

;; int -> [any] -> [any]
(define without
  (lambda (n l)
    (append (take n l) (drop (+ n 1) l))))

;; [int] -> bool
(define consecutive?
  (lambda (l)
    (let ((x (head l))
	  (xs (tail l)))
      (or (null? xs)
	  (and (= (+ x 1) (head xs))
	       (consecutive? xs))))))
;; scheme functions for scsynth operators and ugens

;; ord a => a -> a -> a
(define s:lt
  (lambda (p q)
    (if (< p q) 1 0)))

;; ord a => a -> a -> a
(define s:le
  (lambda (p q)
    (if (<= p q) 1 0)))

;; ord a => a -> a -> a
(define s:ge
  (lambda (p q)
    (if (>= p q) 1 0)))

;; ord a => a -> a -> a
(define s:gt
  (lambda (p q)
    (if (> p q) 1 0)))

;; real -> real -> real
(define s:round
  (lambda (p q)
    (* (round (/ p q)) q)))

;; ord a => a -> a -> a -> a
(define s:clip
  (lambda (a b n)
    (cond ((< n a) a)
	  ((> n b) b)
	  (else n))))

;; number a => a -> a
(define s:squared
  (lambda (n)
    (* n n)))

;; number a => a -> a
(define s:cubed
  (lambda (n)
    (* n n n)))

;; number a => a -> a
(define s:recip
  (lambda (n)
    (/ 1 n)))

;; float -> float
(define s:log2
  (lambda (x)
    (/ (log (abs x)) (log 2))))

;; float -> float
(define s:log10
  (lambda (x)
    (/ (log x) (log 10))))

;; float -> float
(define s:amp-db
  (lambda (x)
    (* (s:log10 x) 20)))

;; float -> float
(define s:db-amp
  (lambda (x)
    (expt 10 (* x 0.05))))

;; float -> float
(define s:pow-db
  (lambda (x)
    (* (s:log10 x) 10)))

;; float -> float
(define s:db-pow
  (lambda (x)
    (expt 10 (* x 0.1))))

;; float -> float
(define s:midi-cps
  (lambda (note)
    (* 440.0 (expt 2.0 (* (- note 69.0) (/ 1.0 12.0))))))

;; float -> float
(define s:cps-midi
  (lambda (freq)
    (+ (* (s:log2 (* freq (/ 1.0 440.0))) 12.0) 69.0)))

;; float -> float
(define s:midi-ratio
  (lambda (midi)
    (expt 2.0 (* midi (/ 1.0 12.0)))))

;; float -> float
(define s:ratio-midi
  (lambda (ratio)
    (* 12.0 (s:log2 ratio))))

;; float -> float
(define s:oct-cps
  (lambda (note)
    (* 440.0 (expt 2.0 (- note 4.75)))))

;; float -> float
(define s:cps-oct
  (lambda (freq)
    (+ (s:log2 (* freq (/ 1.0 440.0))) 4.75)))

;; float -> [float] -> int -> float
(define s:degree-to-key
  (lambda (degree scale steps)
    (let ((scale-n (length scale)))
      (+ (* steps (div degree scale-n))
	 (list-ref scale (exact (mod degree scale-n)))))))

;; (time -> delta-time) -> time -> ()
(define dt-rescheduler
  (lambda (f t)
    (begin
      (pause-thread-until t)
      (let ((r (f t)))
        (when (number? r) (dt-rescheduler f (+ t r)))))))

;; ugen -> ugen -> ugen
;;
;; (hear (mul (sin-osc ar 440 0) (mul (mk-env 5 1) 0.1)))
(define mk-env
  (lambda (s t)
    (let* ((c 4)
           (p (env-linen t s t 1 (list c c c))))
      (env-gen kr 1 1 0 1 remove-synth p))))

;; ugen -> ugen -> ugen -> ugen
;;
;; (audition (with-env (mul (sin-osc ar 440 0) 0.1) 1 0.5))
(define with-env
  (lambda (g s t)
    (out 0 (mul g (mk-env s t)))))

;; overlap-texture-t -> float
(define overlap-texture-iot
  (lambda (s t o)
    (/ (+ (* t 2) s) o)))

;; x|()->x -> x
;;
;; (from-maybe-closure (lambda () 1))
(define from-maybe-closure
  (lambda (x)
    (if (procedure? x) (x) x)))

;; real|#f -> real|#f -> real|()->real -> int -> ugen|()->ugen
(define generalised-texture
  (lambda (s t iot n u)
    (lambda (fd)
      (let ((f (lambda (_)
                 (if (> n 0)
                     (begin
                       (set! n (- n 1))
                       (let* ((u* (from-maybe-closure u))
                              (u** (if (not s) (out 0 u*) (with-env u* s t))))
                         (play-at fd u** -1 add-to-head 1))
                       (from-maybe-closure iot))
                     #f))))
        (dt-rescheduler f (utcr))))))

;; [real|()->real,int] -> ugen|()->ugen -> fd->()
(define spawn-u
  (lambda (k u)
    (let* ((iot (list-ref k 0))
           (n (list-ref k 1)))
      (generalised-texture #f #f iot n u))))

;; overlap-texture-t -> (() -> ugen) -> (fd -> ())
;;
;; (let ((u (pan2 (sin-osc ar (rand 400 600) 0) (rand -1 1) 0.02)))
;;   (with-sc3 (overlap-texture (list 6 3 9 24) u)))
(define overlap-texture
  (lambda (k u)
    (let* ((s (list-ref k 0))
           (t (list-ref k 1))
           (o (list-ref k 2))
           (n (list-ref k 3))
           (iot (overlap-texture-iot s t o)))
      (generalised-texture s t iot n u))))

;; overlap-texture-t -> ugen -> (fd -> ())
(define overlap-texture-u
  (lambda (k u)
    (overlap-texture k (lambda () u))))

;; xfade-texture-t -> float
(define xfade-texture-iot
  (lambda (s t o)
    (/ (+ (* t 2) s) o)))

;; xfade-texture-t -> (() -> ugen) -> (fd -> ())
(define xfade-texture
  (lambda (k u)
    (let* ((s (list-ref k 0))
           (t (list-ref k 1))
           (n (list-ref k 2))
           (iot (+ s t)))
      (generalised-texture s t (lambda () iot) n u))))

;; xfade-texture-t -> ugen -> (fd -> ())
(define xfade-texture-u
  (lambda (k u)
    (xfade-texture k (lambda () u))))

(define post-process-u
  (lambda (nc f)
    (lambda (fd)
      (let ((u (replace-out 0 (f (in nc ar 0)))))
        (play-at fd u -1 add-to-tail 1)))))
