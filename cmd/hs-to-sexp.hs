import System.Environment {- base -}

import Sound.SC3.Lisp.Haskell {- hsc3-lisp -}

hs_to_sexp_usage :: String
hs_to_sexp_usage = "exp|mod [name-rewrite-table] [input-file=/dev/stdin] [output-file=/dev/stdout]"

main :: IO ()
main = do
  a <- getArgs
  let f ty = case ty of
               "exp" -> hs_exp_to_lisp
               "mod" -> hs_to_lisp
               _ -> error hs_to_sexp_usage
  case a of
    [ty] -> hs_to_lisp_f_io (f ty) Nothing "/dev/stdin" "/dev/stdout"
    [ty,i_fn] -> hs_to_lisp_f_io (f ty) Nothing i_fn "/dev/stdout"
    [ty,i_fn,o_fn] -> hs_to_lisp_f_io (f ty) Nothing i_fn o_fn
    [ty,tbl_fn,i_fn,o_fn] -> hs_to_lisp_f_io (f ty) (Just tbl_fn) i_fn o_fn
    _ -> error hs_to_sexp_usage
