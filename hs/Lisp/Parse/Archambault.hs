module Lisp.Parse.Archambault where

import Data.Ratio {- base -}
import Data.Void {- base -}

import qualified Data.Text as T {- text -}

import Math.NumberTheory.Logarithms {- integer-logarithms -}

import Control.Monad.Except {- mtl -}

import qualified Text.Megaparsec as M {- megaparsec -}

import qualified Data.SExpresso.Parse as S {- sexpresso -}
import qualified Data.SExpresso.SExpr as S {- sexpresso -}
import qualified Data.SExpresso.Language.SchemeR5RS as S {- sexpresso -}

import Lisp.Type

type SEXP = S.Datum -- S.SExpr S.SExprType S.SchemeToken

parse_sexp_vm :: String -> VM t [SEXP]
parse_sexp_vm s =
  let f :: String -> Either (M.ParseErrorBundle String Void) [S.SExpr S.SExprType S.SchemeToken]
      f = M.parse (S.decode S.sexpr) ""
  in either (throwError . M.errorBundlePretty) (either throwError return . S.sexpr2Datum) (f s)

-- > map decimal_to_fractional [(123,456),(123456789,123456789)] == [123.456,123456789.123456789]
decimal_to_fractional :: (Integral i, Fractional f) => (i,i) -> f
decimal_to_fractional (n,m) =
  let x = integerLogBase 10 (fromIntegral m)
  in fromIntegral n + (fromIntegral m / fromInteger (10 ^ (x + 1)))

sexp_to_cell :: Lisp_Ty a => SEXP -> VM a (Cell a)
sexp_to_cell sexp =
  let with_sgn :: Num n => S.Sign -> n -> n
      with_sgn x n = if x == S.Plus then n else negate n
  in case sexp of
      S.DNumber (S.SchemeNumber S.Exact (S.CReal (S.SInteger sgn (S.UInteger n)))) -> return (Atom (with_sgn sgn (fromIntegral n)))
      S.DNumber (S.SchemeNumber S.Inexact (S.CReal (S.SDecimal sgn (S.UInteger n) (S.UInteger m) Nothing))) -> return (Atom (with_sgn sgn (decimal_to_fractional (n,m))))
      S.DNumber (S.SchemeNumber S.Inexact (S.CReal (S.SRational sgn (S.UInteger n) (S.UInteger d)))) -> return (Atom (with_sgn sgn (fromRational (n % d))))
      S.DIdentifier nm -> return (Symbol (T.unpack nm))
      S.DQuote (S.DIdentifier nm) -> return (Cons (Symbol "quote") (Cons (Symbol (T.unpack nm)) Nil))  -- quoted...
      S.DString s -> return (String (T.unpack s))
      S.DBoolean b -> return (Atom (ty_from_bool b))
      S.DList [] -> return Nil
      S.DQuote (S.DList []) -> return Nil
      S.DList (e : l) -> sexp_to_cell e >>= \e' -> fmap (Cons e') (sexp_to_cell (S.DList l))
      _ -> throwError ("SEXP-TO-CELL: " ++ show sexp)
