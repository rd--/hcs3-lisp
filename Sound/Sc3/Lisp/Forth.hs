-- | Translate subset of Exp to Forth notation.
module Sound.Sc3.Lisp.Forth where

import Sound.Sc3.Lisp.Scs {- hsc3-lisp -}
import qualified Sound.Sc3.Lisp.Spl as Spl {- hsc3-lisp -}

-- | Define variable
defVar :: String -> Exp -> [String]
defVar n x = ":" : n : expToForth x ++ [";"]

-- | Define word
defWord :: String -> [String] -> [(String, Exp)] -> Exp -> [String]
defWord n p b x = ":" : n : "{" : unwords p : "}" : concatMap (uncurry defVar) b ++ expToForth x ++ [";"]

-- | Exp to Forth.
expToForth :: Exp -> [String]
expToForth e =
  case e of
    Integer x -> [show x]
    Double x -> [show x]
    Symbol x -> [x]
    Array x -> "[" : concatMap expToForth x ++ ["]"]
    Set (Symbol n) (Lambda p (Let b x)) -> defWord n p b x
    Set (Symbol n) x -> defVar n x
    App x y -> concatMap expToForth y ++ expToForth x
    Seq x y -> concatMap expToForth [x, y]
    Define n (Lambda p (Let b x)) -> defWord n p b x
    Define n x -> defVar n x
    _ -> error ("expToForth: " ++ show e)

{- | Stc to Forth.

>>> let rw = stcToFs
>>> rw "f(x, y)"
"x y f"

>>> rw "x.f(y)"
"x y f"

>>> rw "x + y"
"x y +"

>>> rw "[x, y]"
"[ x y ]"

>>> rw "i = f(x) + y;"
": i x f y + ;"

>>> rw "var f = { |x| x * x }; f(3)"
": f { x } x x * ; 3 f"
-}
stcToFs :: String -> String
stcToFs = unwords . concatMap expToForth . Spl.stcToExp True

-- | Spl to Forth
splToFs :: String -> String
splToFs = unwords . concatMap expToForth . Spl.splToExp True
