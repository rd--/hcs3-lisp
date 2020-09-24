module Sound.SC3.Lisp.Parse.Archambault where

import Data.Ratio {- base -}
import Data.Void {- base -}

import qualified Data.Text as T {- text -}

import Math.NumberTheory.Logarithms {- integer-logarithms -}

import Control.Monad.Except {- mtl -}

import qualified Text.Megaparsec as M {- megaparsec -}

import qualified Data.SExpresso.Parse as S {- sexpresso -}
import qualified Data.SExpresso.SExpr as S {- sexpresso -}
import qualified Data.SExpresso.Language.SchemeR5RS as S {- sexpresso -}

import Sound.SC3.Lisp.Type {- hsc3-lisp -}

type SEXP = S.Datum -- S.SExpr S.SExprType S.SchemeToken

parse_sexp_raw :: String -> Either (M.ParseErrorBundle String Void) [S.SExpr S.SExprType S.SchemeToken]
parse_sexp_raw = M.parse (S.decode S.sexpr) ""

-- > parse_sexp_plain "-1 -2.3"
parse_sexp_plain :: String -> Either String [SEXP]
parse_sexp_plain =
  either (Left . M.errorBundlePretty) (either Left Right . S.sexpr2Datum) .
  parse_sexp_raw

-- > parse_sexp_m "(c_set 0 440.0)"
parse_sexp_m :: String -> Maybe [SEXP]
parse_sexp_m = either (const Nothing) Just . parse_sexp_plain

parse_sexp_vm :: String -> VM t [SEXP]
parse_sexp_vm = either throwError return . parse_sexp_plain

-- > map decimal_to_fractional [(123,456),(123456789,123456789)] == [123.456,123456789.123456789]
decimal_to_fractional :: (Integral i, Fractional f) => (i,i) -> f
decimal_to_fractional (n,m) =
  let x = integerLogBase 10 (fromIntegral m)
  in fromIntegral n + (fromIntegral m / fromInteger (10 ^ (x + 1)))

with_sgn :: Num n => S.Sign -> n -> n
with_sgn x n = if x == S.Plus then n else negate n

num_to_cell :: Lisp_Ty a => (S.Exactness, S.Complex) -> Maybe (Cell a)
num_to_cell (ty,c) =
  case (ty,c) of
    (S.Exact,S.CReal (S.SInteger sgn (S.UInteger n))) ->
      Just (Atom (with_sgn sgn (fromIntegral n)))
    (S.Inexact,S.CReal (S.SDecimal sgn (S.UInteger n) (S.UInteger m) Nothing)) ->
      Just (Atom (with_sgn sgn (decimal_to_fractional (n,m))))
    (S.Inexact,S.CReal (S.SRational sgn (S.UInteger n) (S.UInteger d))) ->
      Just (Atom (with_sgn sgn (fromRational (n % d))))
    _ -> Nothing

sexp_to_cell_m :: Lisp_Ty a => SEXP -> Maybe (Cell a)
sexp_to_cell_m sexp =
  case sexp of
    S.DNumber (S.SchemeNumber ty c) -> num_to_cell (ty,c)
    S.DIdentifier nm -> Just (Symbol (T.unpack nm))
    S.DQuote (S.DIdentifier nm) -> Just (quoted_symbol (T.unpack nm))
    S.DString s -> Just (String (T.unpack s))
    S.DBoolean b -> Just (Atom (ty_from_bool b))
    S.DList [] -> Just Nil
    S.DQuote (S.DList []) -> Just Nil
    S.DList (e : l) -> sexp_to_cell_m e >>= \e' -> fmap (Cons e') (sexp_to_cell_m (S.DList l))
    _ -> Nothing

sexp_to_cell :: Lisp_Ty a => SEXP -> VM a (Cell a)
sexp_to_cell sexp =
  let err = throwError ("SEXP-TO-CELL: " ++ show sexp)
  in case sexp of
       S.DNumber (S.SchemeNumber ty c) -> maybe err return (num_to_cell (ty,c))
       S.DIdentifier nm -> return (Symbol (T.unpack nm))
       S.DQuote (S.DIdentifier nm) -> return (quoted_symbol (T.unpack nm))
       S.DString s -> return (String (T.unpack s))
       S.DBoolean b -> return (Atom (ty_from_bool b))
       S.DList [] -> return Nil
       S.DQuote (S.DList []) -> return Nil
       S.DList (e : l) -> sexp_to_cell e >>= \e' -> fmap (Cons e') (sexp_to_cell (S.DList l))
       _ -> err
