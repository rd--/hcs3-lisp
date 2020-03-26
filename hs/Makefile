prefix=$(HOME)/opt
cmd=hsc3-lisp rat-lisp

% : %.hs Lisp.hs
	ghc -Wall -O2 -fno-warn-orphans --make $<

all: $(cmd)

clean:
	rm -f *.hi *.o Lisp/*.hi Lisp/*.o Lisp/Parse/*.hi Lisp/Parse/*.o $(cmd)

install: $(cmd)
	cp -f $(cmd) $(prefix)/bin
