-- | SExp parser using atto-lisp (Schilling).
module Language.Sc3.Lisp.Parse.Schilling where

{-
import Control.Monad.Except {- mtl -}

import qualified Data.Text as T {- text -}
import qualified Data.Text.Encoding as T {- text -}

import qualified Data.Attoparsec.ByteString as A {- attoparsec -}
import qualified Data.Attoparsec.Number as A {- attoparsec -}

import qualified Data.AttoLisp as L {- atto-lisp -}

import Language.Sc3.Lisp.Type {- hsc3-lisp -}

{-
parse_sexp ";lisp\n(1 -1) (0.1 -0.1) (\"x\" x) ((quote x) '()) (#t #f) (λ \"λ\") (x . y)"

s <- readFile "/home/rohan/sw/hsc3-lisp/lisp/rhs.lisp"
parse_sexp s
-}
parse_sexp :: String -> Either String [L.Lisp]
parse_sexp = A.parseOnly (A.many1' L.lisp) . T.encodeUtf8 . T.pack

parse_sexp_vm :: String -> VM t [L.Lisp]
parse_sexp_vm = either throwError return . parse_sexp

sexp_to_exp :: Lisp_Ty a => L.Lisp -> VM a (Exp a)
sexp_to_exp sexp =
  case sexp of
    L.Number (A.I n) -> return (Atom (fromIntegral n))
    L.Number (A.D n) -> return (Atom (realToFrac n))
    L.Symbol s -> case T.unpack s of
      "#f" -> return (Atom (ty_from_bool False))
      "#t" -> return (Atom (ty_from_bool True))
      r -> return (Symbol r)
    L.String s -> return (String (T.unpack s))
    L.List [] -> return Nil
    L.List (e : l) -> sexp_to_exp e >>= \e' -> fmap (Cons e') (sexp_to_exp (L.List l))
    _ -> throwError ("sexp-to-exp: " ++ show sexp)
-}
