-- | Lisp types
module Sound.SC3.Lisp.Type where

import Data.Maybe {- base -}

import qualified Control.Monad.State as State {- mtl -}
import qualified Control.Monad.Except as Except {- mtl -}

import Sound.SC3.Lisp.Env {- hsc3-lisp -}

-- * Types

newtype Trace_Level = Trace_Level Int

-- | Constraints on type parameters for Expr.
class (Eq a,Ord a,Num a,Fractional a) => Lisp_Ty a where
    ty_show :: a -> String -- ^ String representation of /a/, pretty printer.
    ty_to_int :: a -> Int -- ^ Coercion, ie. for Char.
    ty_from_bool :: Bool -> a -- ^ Boolean value represented in /a/, by convention @1@ and @0@.

-- | State monad wrapped in Exception monad.
type EnvMonad m k v r = Except.ExceptT String (State.StateT (Env k v) m) r

-- | Expression.
data Expr a = Symbol String
            | String String
            | Atom a
            | Nil | Cons (Expr a) (Expr a)
            | Fun (Expr a -> Expr a)
            | Proc (Expr a -> EnvMonad IO String (Expr a) (Expr a))
            | Lambda (Env String (Expr a)) String (Expr a)
            | Macro (Expr a)
            | Error String

exp_eq :: Eq a => Expr a -> Expr a -> Bool
exp_eq lhs rhs =
    case (lhs,rhs) of
      (Atom p,Atom q) -> p == q
      (String p,String q) -> p == q
      (Symbol p,Symbol q) -> p == q
      (Nil,Nil) -> True
      (Cons p p',Cons q q') -> p == q && p' == q'
      _ -> False -- error "Eq"

quoted_symbol :: String -> Expr a
quoted_symbol x = (Cons (Symbol "quote") (Cons (Symbol x) Nil))

instance Eq a => Eq (Expr a) where (==) = exp_eq

-- * Instances

is_list :: Eq a => Expr a -> Bool
is_list c =
    case c of
      Cons _ c' -> c' == Nil || is_list c'
      _ -> False

to_list_m :: Lisp_Ty a => Expr a -> Maybe [Expr a]
to_list_m l =
    case l of
      Nil -> Just []
      Cons e l' -> fmap (e :) (to_list_m l')
      _ -> Nothing

to_list :: Lisp_Ty a => Expr a -> [Expr a]
to_list = fromMaybe [Error "NOT LIST?"] . to_list_m

list_pp :: Lisp_Ty a => Expr a -> String
list_pp c = "(" ++ unwords (map show (to_list c)) ++ ")"

instance Lisp_Ty a => Show (Expr a) where
    show c =
        case c of
          Atom a -> ty_show a
          Symbol s -> s
          String s -> show s
          Nil -> "Nil"
          Cons p q -> if is_list c then list_pp c else concat ["(cons ",show p," ",show q,")"]
          Fun _ -> "Fun"
          Proc _ -> "Proc"
          Lambda _ nm code -> concat ["(λ ",nm," ",show code,")"] -- PRIMITIVE λ
          Macro m -> "Macro: " ++ show m
          Error msg -> "Error: " ++ msg
