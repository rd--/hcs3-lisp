clean:
	cabal clean
	rm -Rf dist
	(cd hs; make clean)

push-rd:
	darcs push -a rd@rohandrape.net:sw/hsc3-lisp

pull-rd:
	darcs pull -a http://rohandrape.net/sw/hsc3-lisp
