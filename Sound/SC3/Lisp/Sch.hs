-- | .sch is scheme (or a scheme-like language) written using Haskell notation.
module Sound.SC3.Lisp.Sch where

import Data.Bifunctor {- base -}
import Data.Maybe {- base -}

{- | Ast for Sch notation.
     Lambda, Let and Case have Exp on the left hand side of bindings as some pattern matching is allowed.
-}
data Exp
  = Char Char
  | String String
  | Symbol String
  | Integer Integer
  | Double Double
  | List [Exp]
  | Set Exp Exp
  | App Exp [Exp]
  | Case Exp [(Exp, Exp)]
  | Begin [Exp]
  | If Exp Exp Exp
  | Lambda [Exp] Exp
  | Let [(Exp, Exp)] Exp
  | Tuple [Exp]
  | Define Exp Exp

-- | Apply f at each node of Exp.
exp_map :: (Exp -> Exp) -> Exp -> Exp
exp_map f e =
  case e of
    List p -> List (map (exp_map f) p)
    Set p q -> Set (exp_map f p) (exp_map f q)
    App p q -> App (exp_map f p) (map (exp_map f) q)
    Case p q -> Case (exp_map f p) (map (bimap (exp_map f) (exp_map f)) q)
    Begin p -> Begin (map (exp_map f) p)
    If p q r -> If (exp_map f p) (exp_map f q) (exp_map f r)
    Lambda p q -> Lambda (map (exp_map f) p) (exp_map f q)
    Let p q -> Let (map (bimap (exp_map f) (exp_map f)) p) (exp_map f q)
    Tuple p -> Tuple (map (exp_map f) p)
    Define p q -> Define (exp_map f p) (exp_map f q)
    _ -> f e

-- | Is Exp a Symbol.
is_symbol :: Exp -> Bool
is_symbol e = case e of {Symbol _ -> True; _ -> False}

-- | Is Exp a Lambda expression with no arguments.
is_thunk :: Exp -> Bool
is_thunk e = case e of {Lambda [] _ -> True; _ -> False}

-- | Unpack symbol.
exp_symbol :: Exp -> String
exp_symbol e = case e of {Symbol x -> x; _ -> error "exp_symbol"}

-- * Renaming

-- | Rename all Symbols at Exp using a lookup table.
exp_rename :: [(String, String)] -> Exp -> Exp
exp_rename tbl =
  let rw nm = fromMaybe nm (lookup nm tbl)
      f e =
        case e of
          Symbol s -> Symbol (rw s)
          _ -> e
  in exp_map f

