-- * HUSK-SCHEME <https://github.com/justinethier/husk-scheme>
module Lisp.Parse.Ethier where

import Control.Monad.Except {- mtl -}

import qualified Language.Scheme.Parser as S {- husk-scheme -}
import qualified Language.Scheme.Types as S {- husk-scheme -}

import Lisp.Type {- hsc3-lisp -}

type SEXP = S.LispVal

parse_sexp_vm :: String -> VM a [SEXP]
parse_sexp_vm = either (throwError . show) return . S.readExprList

sexp_to_cell :: Lisp_Ty a => SEXP -> VM a (Cell a)
sexp_to_cell sexp =
    case sexp of
      S.Number n -> return (Atom (fromIntegral n))
      S.Float n -> return (Atom (realToFrac n))
      S.Rational n -> return (Atom (fromRational n))
      S.Atom nm -> return (Symbol nm)
      S.String s -> return (String s)
      S.Bool b -> return (Atom (ty_from_bool b))
      S.List [] -> return Nil
      S.List (e : l) -> sexp_to_cell e >>= \e' -> fmap (Cons e') (sexp_to_cell (S.List l))
      _ -> throwError ("SEXP-TO-CELL: " ++ show sexp)
