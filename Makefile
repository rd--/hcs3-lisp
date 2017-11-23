clean:
	cabal clean
	rm -Rf dist
	(cd hs; make clean)

push-sp:
	darcs push -a rd@slavepianos.org:sw/hsc3-lisp

pull-sp:
	darcs pull -a http://rd.slavepianos.org/sw/hsc3-lisp
