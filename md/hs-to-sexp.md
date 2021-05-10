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
- names are written using lookup table, ie. `sinOsc` may be written as `SinOsc` or `sin-osc`

# Rationale

The immediate reason for this re-writer is to translate
[SuperCollider](http://audiosynth.com) graphs written in haskell for
[hsc3](?t=hsc3) into LISP graphs for [rsc3](?r=rsc3) and [hsc3-lisp](?t=hsc3-lisp).

The haskell notation of the
[analog bubbles](?t=hsc3&e=Help/Graph/jmcc-analog-bubbles.hs) graph
is re-written as:

~~~~
$ hs-to-sexp \
  exp \
  ~/sw/hsc3-lisp/lib/hs-name-tbl.text \
  ~/sw/hsc3/Help/Graph/jmcc-analog-bubbles.hs \
  /dev/stdout
(let ((o (Add (Mul (LFSaw kr (mce2 8 7.23) 0) 3) 80))
      (f (Add (Mul (LFSaw kr 0.4 0) 24) o))
      (s (Mul (SinOsc ar (MIDICPS f) 0) 0.04)))
  (CombN s 0.2 0.2 4))
$
~~~~

The scheme and lisp `non-deterministic` UGen functions generate
identifiers internally, so the written identifiers are removed by a
post-processor, for instance `sed` or `awk`:

The following definitions:

~~~~
lseq l n = dseq 'β' n (mce (map constant l))
lrand l n = drand 'γ' n (mce (map (flip lseq 1) l))
~~~~

translate as:

~~~~
(define lseq (lambda (l n) (dseq n (mce (map constant l)))))
(define lrand (lambda (l n) (drand n (mce (map (flip lseq 1) l)))))
~~~~

and work in `hsc3-lisp`, which is `post-ML`, but not in `scheme`.
