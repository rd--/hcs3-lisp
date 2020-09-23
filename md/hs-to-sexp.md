# sexp (October, 2014)

Translate a subset of [haskell](http://haskell.org) into
`s-expression` (LISP) notation.

- comments, `import` statements and type signatures are discarded
- function application `f x` is written `(f x)`
- function application `f x y` is written `(f x y)` rather than `((f x) y)`
- infix operations `1 + 1` are written `(+ 1 1)`
- local bindings `let x = 1 in x` are written `(let ((x 1)) x)`
- local bindings `let {x = 1;y = 2} in x + y` are written `(let ((x 1) (y 2)) (+ x y))`
- functions `\x -> x * x` are written `(lambda (x) (* x x))`
- functions `\x y -> x * x + y * y` are written `(lambda (x y) (+ (* x x) (* y y)))`
- lists `[1,2,3]` are written `(list 1 2 3)`
- products `(1,2.0,'3',"4")` are written `(list 1 2.0 \#3 "4")`
- ranges `[x .. y]` are written `(enum-from-to x y)`
- series `[x,y .. z]` are written `(enum-from-then-to x y z)`
- conditionals `if x then y else z` are written `(if x y z)`
- module bindings `x = y` are written `(define x y)`
- module bindings `f x = x * x` are written `(define f (lambda (x) (* x x)))`
- the module binding `main = x` is written `x`
- names such as `sinOsc` and `AR` are written in LISP-like forms `sin-osc` and `ar`

# Rationale

The immediate reason for this re-writer is to translate
[SuperCollider](http://audiosynth.com) graphs written in haskell for
[hsc3](?t=hsc3) into LISP graphs for [hsc3-lisp](?t=hsc3-lisp).

The haskell notation of the
[analog bubbles](?t=hsc3-graphs&e=gr/analog-bubbles.scd) graph:

~~~~
analog_bubbles =
  let o = lfSaw KR (mce2 8 7.23) 0 * 3 + 80
      f = lfSaw KR 0.4 0 * 24 + o
      s = sinOsc AR (midiCPS f) 0 * 0.04
  in combN s 0.2 0.2 4

main = audition (out 0 analog_bubbles)
~~~~

is re-written as:

~~~~
(define analog-bubbles
  (let ((o (+ (* (lf-saw kr (mce2 8 7.23) 0) 3) 80))
        (f (+ (* (lf-saw kr 0.4 0) 24) o))
        (s (* (sin-osc ar (midi-cps f) 0) 0.04)))
    (comb-n s 0.2 0.2 4)))

(audition (out 0 analog-bubbles))
~~~~

The [why supercollider?](?t=hsc3-graphs&e=gr/why-supercollider.scd)
graph:

~~~~
why_supercollider =
    let r = resonz (dust 'α' AR 0.2 * 50) (rand 'β' 200 3200) 0.003
        s = mix (uclone 'γ' 10 r)
        z = delayN s 0.048 0.048
        c = combL z 0.1 (lfNoise1 'δ' KR (rand 'ε' 0 0.1) * 0.04 + 0.05) 15
        y = mix (uclone 'ζ' 7 c)
        f i = allpassN i 0.05 (randN 2 'η' 0 0.05) 1
        x = useq 'θ' 4 f y
    in out 0 (s + 0.2 * x)
~~~~

is re-written as:

~~~~
(define why-supercollider
  (let ((r (resonz (* (dust \#α ar 0.2) 50) (rand \#β 200 3200) 0.003))
        (s (mix (uclone \#γ 10 r)))
        (z (delay-n s 0.048 0.048))
        (c (comb-l z 0.1 (+ (* (lf-noise1 \#δ kr (rand \#ε 0 0.1)) 0.04) 0.05) 15))
        (y (mix (uclone \#ζ 7 c)))
        (f (lambda (i) (allpass-n i 0.05 (rand-n 2 \#η 0 0.05) 1)))
        (x (useq \#θ 4 f y)))
    (out 0 (+ s (* 0.2 x)))))
~~~~

The scheme and lisp `non-deterministic` UGen functions generate
identifiers internally, so the written identifiers are removed by a
post-processor, for instance `sed` or `awk`:

~~~~
(define why-supercollider
  (let ((r (resonz (* (dust ar 0.2) 50) (rand 200 3200) 0.003))
        (s (mix (uclone 10 r)))
        (z (delay-n s 0.048 0.048))
        (c (comb-l z 0.1 (+ (* (lf-noise1 kr (rand 0 0.1)) 0.04) 0.05) 15))
        (y (mix (uclone 7 c)))
        (f (lambda (i) (allpass-n i 0.05 (rand-n 2 0 0.05) 1)))
        (x (useq 4 f y)))
    (out 0 (+ s (* 0.2 x)))))
~~~~

The following definitions from
[mridangam-dr.hs](?t=hsc3-graphs&e=gr/mridangam-dr.hs):

~~~~
lseq l n = dseq 'β' n (mce (map constant l))
lrand l n = drand 'γ' n (mce (map (flip lseq 1) l))
~~~~

which translate as:

~~~~
(define lseq (lambda (l n) (dseq n (mce (map constant l)))))
(define lrand (lambda (l n) (drand n (mce (map (flip lseq 1) l)))))
~~~~

work in `HSC3-LISP`, which is a `post-ML` LISP, but not in scheme.
