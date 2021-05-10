GL_GIT=git@gitlab.com:rd--/hsc3-lisp.git
GL_HTTP=https://gitlab.com/rd--/hsc3-lisp.git

all:
	echo "hsc3-lisp"

clean:
	cabal clean
	rm -Rf dist
	(find . -name '*.hi' | xargs rm -f)
	(find . -name '*.o' | xargs rm -f)
	(cd cmd; make clean)

push-gl:
	git push $(GL_GIT)

pull-gl:
	git pull $(GL_HTTP)

update-rd:
	ssh rd@rohandrape.net "(cd sw/hsc3-lisp; git pull $(GL_HTTP))"

push-all:
	make push-gl push-gh update-rd
