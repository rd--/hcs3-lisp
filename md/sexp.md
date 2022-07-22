# sexp (October, 2014)

Translate a subset of [haskell](http://haskell.org) into `s-expression` (Lisp) notation.

- comments, `import` statements, type signatures and type annotations are discarded
- function application
  + `f x` is written `(f x)`
  + `f x y` is written `(f x y)` rather than `((f x) y)`
  + `f ()` is written `(f)`
- infix operations `x + y` are written `(+ x y)`
- local bindings
  + `let x = i in x` is written `(let ((x i)) x)`
  + `let {x = i; y = j} in x + y` is written `(let* ((x i) (y j)) (+ x y))`
  + `let [i,j] = p in (j,i)` is written \
    `(let* ((_rhs p)) (i (listRef _rhs 0)) (j (listRef _rhs 1))) (vector j i))`
  + `let (i,j) = p in (j,i)` is written
    `(let* ((_rhs p)) (i (vectorRef _rhs 0)) (j (vectorRef _rhs 1))) (vector j i))`
- functions
  + `\() -> f ()` is written `(lambda () (f))`
  + `\x -> x * x` is written `(lambda (x) (* x x))`
  + `\x y -> x * x + y * y` is written `(lambda (x y) (+ (* x x) (* y y)))`
  + `\(p, q) r -> p + q * r` is written
    `(lambda (_p1 _p2) (let* ((_rhs _p1) (p (vectorRef _rhs 0)) (q (vectorRef _rhs 1))) (let ((r _p2)) (+ p (* q r)))))`
- lists `[1, 2, 3]` are written `(list 1 2 3)`
- products `(1, 2.0,' 3', "4")` are written `(vector 1 2.0 \#3 "4")`
- ranges `[x .. y]` are written `(enumFromTo x y)`
- series `[x, y .. z]` are written `(enumFromThenTo x y z)`
- conditionals `if x then y else z` are written `(if x y z)`
- cases `\x -> case x of {0 -> a; _ -> b}` are written `(lambda (x) (case x ((0) a) (else b)))`
- right sections `(+ 1)` are written `(lambda (_lhs) (+ _lhs 1))`
- left sections `(1 +)` are written `(lambda (_rhs) (+ 1 _rhs))`
- do expressions
  + `do {display 0; exit 0}` is written `(begin (display 0) (exit 0))`
  + `do {x <- 0; display x}` is written `(begin (set! x 0) (display x)))`
- module bindings
  + `x = y` is written `(define x y)`
  + `f x = x * x` is written `(define f (lambda (x) (* x x)))`
  + `main = x` is written `x`
- a lookup table is consulted and can rewrite `True` as `#t`, `:` as `cons`, `>>` as `begin` &etc.

# Rationale

The immediate reason for this re-writer is to translate
[SuperCollider](http://audiosynth.com) graphs written in haskell for
[hsc3](?t=hsc3) into Lisp graphs for [rsc3](?r=rsc3) and [hsc3-lisp](?t=hsc3-lisp).

There is a table for renaming hsc3 graphs to rsc3 graphs at
`hsc3-lisp/lib/hsc3-name-tbl.text`.  The haskell notation of the
[analog bubbles](?t=hsc3&e=Help/Graph/jmcc-analog-bubbles.hs) graph is
re-written as:

~~~~
$ r.hsc3-to-rsc3.sh < ~/sw/hsc3/Help/Graph/jmcc-analog-bubbles.hs
(let* ((o (Add (Mul (LFSaw kr (Mce2 8 7.23) 0) 3) 80))
       (f (Add (Mul (LFSaw kr 0.4 0) 24) o))
       (s (Mul (SinOsc ar (MIDICPS f) 0) 0.04)))
  (CombN s 0.2 0.2 4))
$
~~~~

The scheme and lisp `non-deterministic` Ugen functions generate
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

* * *

[HS](?t=hsc3-lisp&e=Sound/SC3/Lisp/Haskell.hs)
