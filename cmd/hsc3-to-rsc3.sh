#!/bin/sh
if test $# != 0 ; then echo "hsc3-to-sc3.sh < hsc3-file > rsc3-file" ; exit 1 ; fi
hsc3-sexp haskell-to-lisp --mode=expression --name-rewrite-table=$HOME/sw/hsc3-lisp/lib/hsc3-name-tbl.text
