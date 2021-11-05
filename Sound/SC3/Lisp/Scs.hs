-- | .scs is scheme (or a scheme-like language) written using C-Smalltalk (a subset of SuperCollider) notation.
module Sound.SC3.Lisp.Scs where

import Data.Bifunctor {- base -}
import Data.Maybe {- base -}

-- | Identifier
type Name = String

{- | Ast for .stc as Lisp notation.
     Set has Exp in the binding position because composite and expression assignments are allowed.
-}
data Exp
  = Char Char
  | String String
  | Symbol Name
  | Integer Integer
  | Double Double
  | Array [Exp]
  | Set Exp Exp
  | App Exp [Exp]
  | Seq Exp Exp
  | Let [(Name, Exp)] Exp
  | Lambda [Name] Exp
  | Define Name Exp
  | Nil
  deriving (Eq, Show)

-- | Apply f at each node of Exp.
exp_map :: (Exp -> Exp) -> Exp -> Exp
exp_map f e =
  case e of
    Array p -> Array (map (exp_map f) p)
    Set p q -> Set (exp_map f p) (exp_map f q)
    App p q -> App (exp_map f p) (map (exp_map f) q)
    Seq p q -> Seq (exp_map f p) (exp_map f q)
    Let p q -> Let (map (bimap id (exp_map f)) p) (exp_map f q)
    Lambda p q -> Lambda p (exp_map f q)
    Define p q -> Define p (exp_map f q)
    _ -> f e

-- | Rename Symbol names using lookup table.
exp_rename :: [(String, String)] -> Exp -> Exp
exp_rename tbl =
  let rw nm = fromMaybe nm (lookup nm tbl)
      f e =
        case e of
          Symbol s -> Symbol (rw s)
          _ -> e
  in exp_map f
