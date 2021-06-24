all:
	echo "hsc3-lisp"

mk-cmd:
	(cd cmd ; make install)

clean:
	rm -Rf dist dist-newstyle *~
	(cd cmd; make clean)

push-all:
	r.gitlab-push.sh hsc3-lisp
	r.github-push.sh hsc3-lisp
