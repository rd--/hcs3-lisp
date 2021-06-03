-- | Rewrite a subset of haskell as LISP.
module Sound.SC3.Lisp.Haskell where

import Data.Maybe {- base -}

import qualified Language.Haskell.Exts as E {- haskell-src-exts -}

import qualified Language.Scheme.Types as S {- husk-scheme -}

import qualified Sound.SC3.Lisp.Parse.Ethier as L {- hsc3-lisp -}

error_x :: Show a => String -> a -> t
error_x nm x = error (nm ++ ": " ++ show x)

-- | Name re-writing table, from LHS to RHS.
type Name_Table = [(String,String)]

-- | Rewrite names using table lookup.
--
-- > name_rewrite_table [("sinOsc","SinOsc")] "sinOsc" == "SinOsc"
name_rewrite_table :: Name_Table -> String -> String
name_rewrite_table tbl nm = fromMaybe nm (lookup nm tbl)

-- | Names are re-written using a lookup table.
name_str :: Name_Table -> E.Name l -> String
name_str tbl nm =
    case nm of
      E.Symbol _ s -> name_rewrite_table tbl s
      E.Ident _ s -> name_rewrite_table tbl s

qname_str :: Show l => Name_Table -> E.QName l -> String
qname_str tbl q =
    case q of
      E.UnQual _ nm -> name_str tbl nm
      E.Special _ (E.Cons _) -> "cons"
      _ -> error_x "qname_str" q

qop_str :: Show l => Name_Table -> E.QOp l -> String
qop_str tbl q =
    case q of
      E.QVarOp _ nm -> qname_str tbl nm
      E.QConOp _ nm -> qname_str tbl nm

rhs_exp :: Show l => E.Rhs l -> E.Exp l
rhs_exp r =
    case r of
      E.UnGuardedRhs _ e -> e
      _ -> error_x "rhs_exp" r

literal_sexp :: Show l => E.Literal l -> L.SExp
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

pat_sexp :: Show l => Name_Table -> E.Pat l -> L.SExp
pat_sexp tbl p =
    case p of
      E.PVar _ nm -> S.Atom (name_str tbl nm)
      E.PLit _ sgn lit -> literal_sexp (if sign_is_negative sgn then literal_negate lit else lit)
      _ -> error_x "pat_sexp" p

alt_sexp :: Show l => Name_Table -> E.Alt l -> L.SExp
alt_sexp tbl alt =
  case alt of
    E.Alt _ (E.PWildCard _) rhs Nothing -> S.List [S.Atom "else",exp_sexp tbl (rhs_exp rhs)]
    E.Alt _ lhs rhs Nothing -> S.List [S.List [pat_sexp tbl lhs],exp_sexp tbl (rhs_exp rhs)]
    _ -> error_x "alt: bindings?" alt

-- | Tuples map to lists but could be dotted lists or vectors.
exp_sexp :: Show l => Name_Table -> E.Exp l -> L.SExp
exp_sexp tbl e =
    case e of
      E.App _ f x -> S.List (map (exp_sexp tbl) (unwind_app (f,x)))
      E.Case _ c a -> S.List (S.Atom "case" : exp_sexp tbl c : map (alt_sexp tbl) a)
      E.Con _ nm -> S.Atom (qname_str tbl nm)
      E.EnumFromTo _ p q -> S.List (S.Atom "enum-from-to" : map (exp_sexp tbl) [p,q])
      E.EnumFromThenTo _ p q r -> S.List (S.Atom "enum-from-then-to" : map (exp_sexp tbl) [p,q,r])
      E.If _ p q r -> S.List (S.Atom "if" : map (exp_sexp tbl) [p,q,r])
      E.InfixApp _ lhs qop rhs -> S.List [S.Atom (qop_str tbl qop),exp_sexp tbl lhs,exp_sexp tbl rhs]
      E.Lambda _ p c -> S.List [S.Atom "lambda",S.List (map (pat_sexp tbl) p),exp_sexp tbl c]
      E.Let _ b e' -> S.List [S.Atom "letrec",S.List (map (decl_sexp tbl) (binds_decl (Just b))),exp_sexp tbl e']
      E.List _ l -> S.List (S.Atom "list" : map (exp_sexp tbl) l)
      E.Lit _ l -> literal_sexp l
      E.NegApp _ n ->
          case n of
            E.Lit _ l -> literal_sexp (literal_negate l)
            _ -> S.List [S.Atom "negate",exp_sexp tbl n]
      E.Paren _ e' -> exp_sexp tbl e'
      E.Tuple _ _ l -> S.List (S.Atom "list" : map (exp_sexp tbl) l)
      E.Var _ nm -> S.Atom (qname_str tbl nm)
      E.ExpTypeSig _ e' _ -> exp_sexp tbl e' -- discard type annotation
      _ -> error_x "exp_sexp: unimplemented expression type" e

binds_decl :: Show l => Maybe (E.Binds l) -> [E.Decl l]
binds_decl b =
    case b of
      Nothing -> []
      Just (E.BDecls _ d) -> d
      Just (E.IPBinds _ _) -> error_x "binds_decl" b

pat_var_str :: Show l => Name_Table -> E.Pat l -> String
pat_var_str tbl p =
    case p of
      E.PVar _ nm -> name_str tbl nm
      _ -> error_x "pat_var_str" p

match_sexp :: Show l => Name_Table -> E.Match l -> (L.SExp,L.SExp)
match_sexp tbl m =
    case m of
      E.Match _ nm param rhs Nothing ->
          let nm' = S.Atom (name_str tbl nm)
              param' = map (pat_sexp tbl) param
              rhs' = exp_sexp tbl (rhs_exp rhs)
          in (nm',S.List [S.Atom "lambda",S.List param',rhs'])
      _ -> error_x "match_sexp" m

decl_sexp :: Show l => Name_Table -> E.Decl l -> L.SExp
decl_sexp tbl d =
    case d of
      E.FunBind _ [m] -> let (nm,rhs) = match_sexp tbl m in S.List [nm,rhs]
      E.PatBind _ lhs rhs bnd ->
          case binds_decl bnd of
            [] -> S.List [pat_sexp tbl lhs,exp_sexp tbl (rhs_exp rhs)]
            _ -> error_x "decl_sexp" bnd
      _ -> error_x "decl_sexp" d

mod_decl_sexp :: Show l => Name_Table -> E.Decl l -> Maybe L.SExp
mod_decl_sexp tbl d =
    case d of
      E.FunBind _ [m] ->
          let (nm,rhs) = match_sexp tbl m
          in Just (S.List [S.Atom "define",nm,rhs])
      E.PatBind _ lhs rhs bnd ->
          case binds_decl bnd of
            [] -> case pat_var_str tbl lhs of
                    "main" -> Just (exp_sexp tbl (rhs_exp rhs))
                    nm -> Just (S.List [S.Atom "define",S.Atom nm,exp_sexp tbl (rhs_exp rhs)])
            _ -> error_x "mod_decl_sexp" bnd
      E.TypeSig _ _ _ -> Nothing
      _ -> error_x "mod_decl_sexp" d

module_decl :: Show l => E.Module l -> [E.Decl l]
module_decl m =
    case m of
      E.Module _ _ _ _ d -> d
      _ -> error_x "mod_decl" m

hs_exp_sexp :: Name_Table -> String -> L.SExp
hs_exp_sexp tbl s =
    case E.parseExp s of
      E.ParseOk e -> exp_sexp tbl e
      err -> error_x "hs_read_exp" err

{- | Haskell expression to s-expression.

> let rw = hs_exp_to_lisp []
> rw "f x" == "(f x)"
> rw "f x y" == "(f x y)"
> rw "x + y" == "(+ x y)"
> rw "let x = y in x" == "(letrec ((x y)) x)"
> rw "let {x = i;y = j} in x + y" == "(letrec ((x i) (y j)) (+ x y))"
> rw "\\x -> x * x" == "(lambda (x) (* x x))"
> rw "\\x y -> x * x + y * y" == "(lambda (x y) (+ (* x x) (* y y)))"
> rw "[1,2,3]" == "(list 1 2 3)"
> rw "(1,2.0,'3',\"4\")" == "(list 1 2.0 #\\3 \"4\")"
> rw "[x .. y]" == "(enum-from-to x y)"
> rw "[x,y .. z]" == "(enum-from-then-to x y z)"
> rw "if x then y else z" == "(if x y z)"
> rw "\\x -> case x of {0 -> a;1 -> b;_ -> c}" == "(lambda (x) (case x ((0) a) ((1) b) (else c)))"
-}
hs_exp_to_lisp :: Name_Table -> String -> String
hs_exp_to_lisp tbl = L.sexp_show . hs_exp_sexp tbl

{- | Rewrite Haskell declaration to s-expression

> let rw = L.sexp_show . hs_decl_sexp []
> rw "x = y" == "(define x y)"
> rw "f x = case x of {0 -> a;1 -> b;_ -> c}" == "(define f (lambda (x) (case x ((0) a) ((1) b) (else c))))"
> rw "x = y" == "(define x y)"
> rw "f x = x * x" == "(define f (lambda (x) (* x x)))"
> rw "main = x" == "x"
-}
hs_decl_sexp :: Name_Table -> String -> L.SExp
hs_decl_sexp tbl s =
    case E.parseDecl s of
      E.ParseOk d -> fromMaybe (error "hs_decl_sexp") (mod_decl_sexp tbl d)
      err -> error_x "hs_decl_sexp" err

-- | Haskell module source to list of 'L.SExp'.
hs_module_sexp :: Name_Table -> String -> [L.SExp]
hs_module_sexp tbl s =
    case E.parseModule s of
      E.ParseOk m -> mapMaybe (mod_decl_sexp tbl) (module_decl m)
      err -> error_x "hs_read_module" err

{- | Translate haskell @module@ code into @LISP@.

> let rw = hs_to_lisp []
> rw "import Sound.SC3" == ""
> rw "o = let f = midiCPS (mce [65.0,65.1]) in sinOsc AR f 0"
> rw "a = dbAmp (-24)"
> rw "main = audition (out 0 (o * a))"
> rw "x = (1,2,3)"
> rw "sq n = n * n"
> rw "abs n = if n >= 0 then n else -n"
> rw "l = [0 .. 9]"
> rw "l = [0, 2 .. 8]"
> rw "n = 0.1 * 0.01 * 0.001"
> rw "l = [1,1.0,\"str\",'c']"
> rw "sq = \\x -> x * x"
> rw "sum_sq = \\x y -> x * x + y * y"
> rw "l = 1 : []"
> rw "main = putStrLn \"text\""
-}
hs_to_lisp :: Name_Table -> String -> String
hs_to_lisp tbl = unlines . map L.sexp_show . hs_module_sexp tbl

-- | Load table given name re-writing rules, one per line.
name_tbl_load :: FilePath -> IO Name_Table
name_tbl_load fn = do
  txt <- readFile fn
  let parse x = case words x of
                  [lhs,rhs] -> (lhs,rhs)
                  _ -> error ("name_tbl_load: " ++ x)
  return (map parse (lines txt))

hs_to_lisp_f_io :: (Name_Table -> String -> String) -> Maybe FilePath -> FilePath -> FilePath -> IO ()
hs_to_lisp_f_io proc_f tbl_fn i_fn o_fn = do
  tbl <- maybe (return []) name_tbl_load tbl_fn
  i <- readFile i_fn
  writeFile o_fn (proc_f tbl i)

hs_to_lisp_io :: Maybe FilePath -> FilePath -> FilePath -> IO ()
hs_to_lisp_io = hs_to_lisp_f_io hs_to_lisp

hs_exp_to_lisp_io :: Maybe FilePath -> FilePath -> FilePath -> IO ()
hs_exp_to_lisp_io = hs_to_lisp_f_io hs_exp_to_lisp
