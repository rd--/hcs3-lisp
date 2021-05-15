module Sound.SC3.Lisp.Type where

import Data.IORef {- base -}
import Data.Maybe {- base -}

import qualified Control.Monad.State as Monad {- mtl -}
import qualified Control.Monad.Except as Monad {- mtl -}

import qualified Data.Map as Map {- containers -}

import Sound.SC3.Lisp.Env {- hsc3-lisp -}

-- * Types

class (Eq a,Ord a,Num a,Fractional a) => Lisp_Ty a where
    ty_show :: a -> String -- ^ String representation of /a/, pretty printer.
    ty_to_int :: a -> Int -- ^ Coercion, ie. for Char.
    ty_from_bool :: Bool -> a -- ^ Boolean value represented in /a/, by convention @1@ and @0@.

type Trace_Level = Int

data Cell a = Symbol String | String String
            | Atom a
            | Nil | Cons (Cell a) (Cell a)
            | Fun (Cell a -> Cell a)
            | Proc (Cell a -> VM (Cell a) (Cell a))
            | Lambda (Env (Cell a)) String (Cell a)
            | Macro (Cell a)
            | Error String

cell_eq :: Eq a => Cell a -> Cell a -> Bool
cell_eq lhs rhs =
    case (lhs,rhs) of
      (Atom p,Atom q) -> p == q
      (String p,String q) -> p == q
      (Symbol p,Symbol q) -> p == q
      (Nil,Nil) -> True
      (Cons p p',Cons q q') -> p == q && p' == q'
      _ -> False -- error "EQ"

quoted_symbol :: String -> Cell a
quoted_symbol x = (Cons (Symbol "quote") (Cons (Symbol x) Nil))

instance Eq a => Eq (Cell a) where (==) = cell_eq

-- * Instances

is_list :: Eq a => Cell a -> Bool
is_list c =
    case c of
      Cons _ c' -> c' == Nil || is_list c'
      _ -> False

to_list_m :: Lisp_Ty a => Cell a -> Maybe [Cell a]
to_list_m l =
    case l of
      Nil -> Just []
      Cons e l' -> fmap (e :) (to_list_m l')
      _ -> Nothing

to_list :: Lisp_Ty a => Cell a -> [Cell a]
to_list = fromMaybe [Error "NOT LIST?"] . to_list_m

list_pp :: Lisp_Ty a => Cell a -> String
list_pp c = "(" ++ unwords (map show (to_list c)) ++ ")"

instance Lisp_Ty a => Show (Cell a) where
    show c =
        case c of
          Atom a -> ty_show a
          Symbol s -> s
          String s -> show s
          Nil -> "nil"
          Cons p q -> if is_list c then list_pp c else concat ["(cons ",show p," ",show q,")"]
          Fun _ -> "FUN"
          Proc _ -> "PROC"
          Lambda _ nm code -> concat ["(λ ",nm," ",show code,")"] -- PRIMITIVE λ
          Macro m -> "MACRO: " ++ show m
          Error msg -> "ERROR: " ++ msg
