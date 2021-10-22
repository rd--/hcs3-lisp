module Sound.SC3.Lisp.Sch where

type Name = String

data Exp
  = Char Char
  | String String
  | Integer Integer
  | Double Double
  | List [Exp]
  | Symbol String
  | Set Exp Exp
  | App Exp [Exp]
  | Case Exp [(Exp,Exp)]
  | Begin [Exp]
  | If Exp Exp Exp
  | Lambda [Exp] Exp
  | Let [(Exp, Exp)] Exp
  | Tuple [Exp]
  | Define Exp Exp


