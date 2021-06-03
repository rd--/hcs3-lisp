import qualified Music.Theory.Opt as Opt {- hmt -}

import Sound.SC3.Lisp.Haskell {- hsc3-lisp -}

opt :: [Opt.OptUsr]
opt =
  [("input-file","/dev/stdin","string","haskell input file")
  ,("output-file","/dev/stdout","string","scheme output file")
  ,("name-rewrite-table","nil","string","name rewriting table")
  ,("mode","expr","string","parser mode expr=expression mod=module")]

help :: Opt.OptHelp
help = ["hs-to-sexp [opt]"]

main :: IO ()
main = do
  (o,_) <- Opt.opt_get_arg True help opt
  let usage = Opt.opt_error help opt
      mode x = case x of
                 "expr" -> hs_exp_to_lisp
                 "mod" -> hs_to_lisp
                 _ -> usage
      table x = if x == "nil" then Nothing else Just x
  hs_to_lisp_f_io
    (mode (Opt.opt_get o "mode"))
    (table (Opt.opt_get o "name-rewrite-table"))
    (Opt.opt_get o "input-file")
    (Opt.opt_get o "output-file")
