-- | .sch files are scheme (or a scheme-like) language written using Haskell notation.
module Sound.SC3.Lisp.Sch where

-- | Ast for Sch notation.
data Exp
  = Char Char
  | String String
  | Integer Integer
  | Double Double
  | List [Exp]
  | Symbol String
  | Set Exp Exp
  | App Exp [Exp]
  | Case Exp [(Exp, Exp)]
  | Begin [Exp]
  | If Exp Exp Exp
  | Lambda [Exp] Exp
  | Let [(Exp, Exp)] Exp
  | Tuple [Exp]
  | Define Exp Exp

-- | Is Exp a Symbol.
is_symbol :: Exp -> Bool
is_symbol e = case e of {Symbol _ -> True; _ -> False}

-- | Is Exp a Lambda expression with no arguments.
is_thunk :: Exp -> Bool
is_thunk e = case e of {Lambda [] _ -> True; _ -> False}

-- | Unpack symbol.
exp_symbol :: Exp -> String
exp_symbol e = case e of {Symbol x -> x; _ -> error "exp_symbol"}
