prefix=$(HOME)/opt
cmd=hsc3-lisp rat-lisp hs-to-sexp

% : %.hs
	ghc -Wall -O2 -fno-warn-orphans --make $<

all: $(cmd)

clean:
	rm -f *.hi *.o $(cmd)

install: $(cmd)
	cp -f $(cmd) $(prefix)/bin
