## Haskell Syntax -> S-expression

Hello List

I'm posting this here because when looking for information about non S-expression syntax for Lisps I came across the Racket2 threads.

Recently I've been using a simple Haskell Syntax -> S-expression translator to work interactively with a Scheme interpreter and I'm surprised by how pleasant it's been.

I don't really know if or how this might relate to the Racket2 situation, I'm sure wholesale adoption of an existing syntax, parser and abstract syntax tree is a very uninteresting idea in that context.

But for some problem domains, and for people who already know Haskell, this may be useful?

It is, in any case, very simple to implement.

The Haskell module is at:

https://gitlab.com/rd--/hsc3-lisp/-/blob/master/Sound/SC3/Lisp/Haskell.hs

There's a brief note about the translation rules at:

https://gitlab.com/rd--/hsc3-lisp/-/blob/master/md/sexp.md

Best,
Rohan

[racket-users](https://groups.google.com/g/racket-users/c/HD4fP4QprGI)
