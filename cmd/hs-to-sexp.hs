import System.Environment {- base -}

import Sound.SC3.Lisp.Haskell {- hsc3-lisp -}

main :: IO ()
main = do
  a <- getArgs
  case a of
    [] -> hs_to_lisp_io "/dev/stdin" "/dev/stdout"
    [i_fn] -> hs_to_lisp_io i_fn "/dev/stdout"
    [i_fn,o_fn] -> hs_to_lisp_io i_fn o_fn
    _ -> error "[input-file=/dev/stdin] [output-file=/dev/stdout]"
