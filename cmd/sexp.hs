import Control.Monad {- base -}

import qualified Music.Theory.Opt as Opt {- hmt-base -}

import qualified Sound.SC3.Lisp.Haskell as Lisp {- hsc3-lisp -}

opt :: [Opt.OptUsr]
opt =
  [("input-file","/dev/stdin","string","haskell input file")
  ,("output-file","/dev/stdout","string","scheme output file")
  ,("name-rewrite-table","nil","string","name rewriting table")
  ,("mode","module","string","parser mode, module|expression")]

help :: Opt.OptHelp
help = ["hsc3-sexp haskell-to-lisp [opt]"]

main :: IO ()
main = do
  (o,a) <- Opt.opt_get_arg True help opt
  let usage = Opt.opt_error help opt
      mode x = case x of
                 "expression" -> Lisp.hs_exp_to_lisp
                 "module" -> Lisp.hs_to_lisp
                 _ -> usage
      table x = if x == "nil" then Nothing else Just x
  when (a /= ["haskell-to-lisp"]) usage
  Lisp.hs_to_lisp_f_io
    (mode (Opt.opt_get o "mode"))
    (table (Opt.opt_get o "name-rewrite-table"))
    (Opt.opt_get o "input-file")
    (Opt.opt_get o "output-file")
