rhs_dir=$(HOME)/sw/rhs/src
rsc3_dir=$(HOME)/sw/rsc3/src
rsc3_lang_dir=$(HOME)/sw/rsc3-lang/src
lisp_dir=$(HOME)/sw/hsc3-lisp/lisp

rhs_lib=control/monad.scm data/bool.scm data/ord.scm data/function.scm data/list.scm data/tree.scm data/tree.scm prelude.scm
rsc3_lib=command.scm constant.scm derived.scm envelope.scm list.scm sc3.scm

rhs:
	(cd $(rhs_dir) ; cat $(rhs_lib) > $(lisp_dir)/rhs.lisp)

rsc3:
	(cd $(rsc3_dir) ; cat $(rsc3_lib) > $(lisp_dir)/rsc3.lisp)
	cat $(rsc3_lang_dir)/overlap-texture.scm >> $(lisp_dir)/rsc3.lisp
	cp $(rsc3_dir)/ugen.scm $(lisp_dir)/ugen.lisp

all: rhs rsc3

clean:
	rm -f rhs.lisp rsc3.lisp
