all:
	echo "hsc3-lisp"

clean:
	cabal clean
	rm -Rf dist
	(find . -name '*.hi' | xargs rm -f)
	(find . -name '*.o' | xargs rm -f)
	(cd cmd; make clean)

push-rd:
	darcs push -a rd@rohandrape.net:sw/hsc3-lisp

pull-rd:
	darcs pull -a http://rohandrape.net/sw/hsc3-lisp
