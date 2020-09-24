-- | Rewrite a subset of haskell as LISP.
module Sound.SC3.Lisp.Haskell where

import Data.Maybe {- base -}
import qualified Numeric {- base -}

import qualified Language.Haskell.Exts as E {- haskell-src-exts -}
import qualified Language.Scheme.Types as S {- husk-scheme -}

import qualified Sound.SC3 {- hsc3 -}

error_x :: Show a => String -> a -> t
error_x nm x = error (nm ++ ": " ++ show x)

type SEXP = S.LispVal

{- | The HUSK printer uses "show" for Floats, and prints 'Char' directly.

> (S.Float 0.01,S.Char 'c') -- 1.0e-2 c
> mapM_ (putStrLn . show_sexp) [S.Float 0.01,S.Char 'c'] -- 0.01 #\c
-}
show_sexp :: SEXP -> String
show_sexp s =
    case s of
      S.Atom x -> x
      S.Char x -> ['#','\\',x]
      S.Float x -> Numeric.showFFloat Nothing x ""
      S.List x -> "(" ++ unwords (map show_sexp x) ++ ")"
      S.Number x -> show x
      S.String x -> "\"" ++ x ++ "\""
      _ -> error_x "show_sexp" s

-- | Names are re-written from SC3 form (sinOsc) to LISP form (sin-osc).
name_str :: E.Name l -> String
name_str nm =
    case nm of
      E.Symbol _ s -> Sound.SC3.sc3_name_to_lisp_name s
      E.Ident _ s -> Sound.SC3.sc3_name_to_lisp_name s

qname_str :: Show l => E.QName l -> String
qname_str q =
    case q of
      E.UnQual _ nm -> name_str nm
      E.Special _ (E.Cons _) -> "cons"
      _ -> error_x "qname_str" q

qop_str :: Show l => E.QOp l -> String
qop_str q =
    case q of
      E.QVarOp _ nm -> qname_str nm
      E.QConOp _ nm -> qname_str nm

rhs_exp :: Show l => E.Rhs l -> E.Exp l
rhs_exp r =
    case r of
      E.UnGuardedRhs _ e -> e
      _ -> error_x "rhs_exp" r

literal_sexp :: Show l => E.Literal l -> SEXP
literal_sexp l =
    case l of
      E.Char _ c _ -> S.Char c
      E.String _ s _ -> S.String s
      E.Int _ i _ -> S.Number i
      E.Frac _ r _ -> S.Float (fromRational r)
      _ -> error_x "literal_sexp" l

literal_negate :: Show l => E.Literal l -> E.Literal l
literal_negate lit =
    case lit of
      E.Int l i _ -> E.Int l (- i) "..."
      E.Frac l r _ -> E.Frac l (- r) "..."
      _ -> error_x "literal_negate" lit

unwind_app :: (E.Exp l,E.Exp l) -> [E.Exp l]
unwind_app (lhs,rhs) =
    case lhs of
      E.App _ p q -> unwind_app (p,q) ++ [rhs]
      _ -> [lhs,rhs]

sign_is_negative :: E.Sign l -> Bool
sign_is_negative sgn =
  case sgn of
    E.Signless _ -> False
    E.Negative _ -> True

pat_sexp :: Show l => E.Pat l -> SEXP
pat_sexp p =
    case p of
      E.PVar _ nm -> S.Atom (name_str nm)
      E.PLit _ sgn lit -> literal_sexp (if sign_is_negative sgn then literal_negate lit else lit)
      _ -> error_x "pat_sexp" p

alt_sexp :: Show l => E.Alt l -> SEXP
alt_sexp alt =
  case alt of
    E.Alt _ (E.PWildCard _) rhs Nothing -> S.List [S.Atom "else",exp_sexp (rhs_exp rhs)]
    E.Alt _ lhs rhs Nothing -> S.List [S.List [pat_sexp lhs],exp_sexp (rhs_exp rhs)]
    _ -> error_x "alt: bindings?" alt

-- | Tuples map to lists but could be dotted lists or vectors.
exp_sexp :: Show l => E.Exp l -> SEXP
exp_sexp e =
    case e of
      E.App _ f x -> S.List (map exp_sexp (unwind_app (f,x)))
      E.Case _ c a -> S.List (S.Atom "case" : exp_sexp c : map alt_sexp a)
      E.Con _ nm -> S.Atom (qname_str nm)
      E.EnumFromTo _ p q -> S.List (S.Atom "enum-from-to" : map exp_sexp [p,q])
      E.EnumFromThenTo _ p q r -> S.List (S.Atom "enum-from-then-to" : map exp_sexp [p,q,r])
      E.If _ p q r -> S.List (S.Atom "if" : map exp_sexp [p,q,r])
      E.InfixApp _ lhs qop rhs -> S.List [S.Atom (qop_str qop),exp_sexp lhs,exp_sexp rhs]
      E.Lambda _ p c -> S.List [S.Atom "lambda",S.List (map pat_sexp p),exp_sexp c]
      E.Let _ b e' -> S.List [S.Atom "let",S.List (map decl_sexp (binds_decl (Just b))),exp_sexp e']
      E.List _ l -> S.List (S.Atom "list" : map exp_sexp l)
      E.Lit _ l -> literal_sexp l
      E.NegApp _ n ->
          case n of
            E.Lit _ l -> literal_sexp (literal_negate l)
            _ -> S.List [S.Atom "negate",exp_sexp n]
      E.Paren _ e' -> exp_sexp e'
      E.Tuple _ _ l -> S.List (S.Atom "list" : map exp_sexp l)
      E.Var _ nm -> S.Atom (qname_str nm)
      _ -> error_x "exp_sexp" e

binds_decl :: Show l => Maybe (E.Binds l) -> [E.Decl l]
binds_decl b =
    case b of
      Nothing -> []
      Just (E.BDecls _ d) -> d
      Just (E.IPBinds _ _) -> error_x "binds_decl" b

pat_var_str :: Show l => E.Pat l -> String
pat_var_str p =
    case p of
      E.PVar _ nm -> name_str nm
      _ -> error_x "pat_var_str" p

match_sexp :: Show l => E.Match l -> (SEXP,SEXP)
match_sexp m =
    case m of
      E.Match _ nm param rhs Nothing ->
          let nm' = S.Atom (name_str nm)
              param' = map pat_sexp param
              rhs' = exp_sexp (rhs_exp rhs)
          in (nm',S.List [S.Atom "lambda",S.List param',rhs'])
      _ -> error_x "match_sexp" m

decl_sexp :: Show l => E.Decl l -> SEXP
decl_sexp d =
    case d of
      E.FunBind _ [m] -> let (nm,rhs) = match_sexp m in S.List [nm,rhs]
      E.PatBind _ lhs rhs bnd ->
          case binds_decl bnd of
            [] -> S.List [pat_sexp lhs,exp_sexp (rhs_exp rhs)]
            _ -> error_x "decl_sexp" bnd
      _ -> error_x "decl_sexp" d

mod_decl_sexp :: Show l => E.Decl l -> Maybe SEXP
mod_decl_sexp d =
    case d of
      E.FunBind _ [m] ->
          let (nm,rhs) = match_sexp m
          in Just (S.List [S.Atom "define",nm,rhs])
      E.PatBind _ lhs rhs bnd ->
          case binds_decl bnd of
            [] -> case pat_var_str lhs of
                    "main" -> Just (exp_sexp (rhs_exp rhs))
                    nm -> Just (S.List [S.Atom "define",S.Atom nm,exp_sexp (rhs_exp rhs)])
            _ -> error_x "mod_decl_sexp" bnd
      E.TypeSig _ _ _ -> Nothing
      _ -> error_x "mod_decl_sexp" d

module_decl :: Show l => E.Module l -> [E.Decl l]
module_decl m =
    case m of
      E.Module _ _ _ _ d -> d
      _ -> error_x "mod_decl" m

-- > putStrLn $ show_sexp $ hs_exp_sexp "print x"
-- > putStrLn $ show_sexp $ hs_exp_sexp "\\x -> case x of {0 -> 'a';1 -> 'b';_ -> 'c'}"
hs_exp_sexp :: String -> SEXP
hs_exp_sexp s =
    case E.parseExp s of
      E.ParseOk e -> exp_sexp e
      err -> error_x "hs_read_exp" err

-- > putStrLn $ show_sexp $ hs_decl_sexp "x = 5"
-- > putStrLn $ show_sexp $ hs_decl_sexp "f x = case x of {0 -> 'a';1 -> 'b';_ -> 'c'}"
hs_decl_sexp :: String -> SEXP
hs_decl_sexp s =
    case E.parseDecl s of
      E.ParseOk d -> fromMaybe (error "hs_decl_sexp") (mod_decl_sexp d)
      err -> error_x "hs_decl_sexp" err

{- | Haskell module source to list of 'SEXP'.

> let m = ["import Sound.SC3"
>         ,"o = let f = midiCPS (mce [65.0,65.1]) in sinOsc AR f 0"
>         ,"a = dbAmp (-24)"
>         ,"main = audition (out 0 (o * a))"]
> in hs_modeul_sexp (unlines m)

> > (define o (let ((f (midiCPS (mce (list 65.0 65.1))))) (sinOsc AR f 0)))
> > (define a (dbAmp -24))
> > (define main (audition (out 0 (* o a))))

-}
hs_module_sexp :: String -> [SEXP]
hs_module_sexp s =
    case E.parseModule s of
      E.ParseOk m -> mapMaybe mod_decl_sexp (module_decl m)
      err -> error_x "hs_read_module" err

{- | Translate haskell @module@ code into @LISP@.

> let i = ["x = (1,2,3)"
>         ,"sq n = n * n"
>         ,"abs n = if n >= 0 then n else -n"
>         ,"l = [0 .. 9]"
>         ,"l = [0, 2 .. 8]"
>         ,"n = 0.1 * 0.01 * 0.001"
>         ,"l = [1,1.0,\"str\",'c']"
>         ,"sq = \\x -> x * x"
>         ,"sum_sq = \\x y -> x * x + y * y"
>         ,"l = 1 : []"
>         ,"main = putStrLn \"text\""]
> in map hs_to_lisp i

-}
hs_to_lisp :: String -> String
hs_to_lisp = unlines . map show_sexp . hs_module_sexp

hs_to_lisp_io :: FilePath -> FilePath -> IO ()
hs_to_lisp_io i_fn o_fn = do
  i <- readFile i_fn
  writeFile o_fn (hs_to_lisp i)
