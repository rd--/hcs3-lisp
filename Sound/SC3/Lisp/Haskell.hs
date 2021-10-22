-- | Rewrite a subset of haskell as LISP.
module Sound.SC3.Lisp.Haskell where

import Data.Maybe {- base -}

import qualified Language.Haskell.Exts as E {- haskell-src-exts -}

import qualified Language.Scheme.Types as S {- husk-scheme -}

import qualified Sound.SC3.Lisp.Parse.Ethier as L {- hsc3-lisp -}
import qualified Sound.SC3.Lisp.Sch as Sch {- hsc3-lisp -}

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
      E.Special _ (E.UnitCon _) -> "unit"
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

literal_sch :: Show l => E.Literal l -> Sch.Exp
literal_sch l =
  case l of
    E.Char _ c _ -> Sch.Char c
    E.String _ s _ -> Sch.String s
    E.Int _ i _ -> Sch.Integer i
    E.Frac _ r _ -> Sch.Double (fromRational r)
    _ -> error_x "literal" l

literal_negate :: Show l => E.Literal l -> E.Literal l
literal_negate lit =
    case lit of
      E.Int l i _ -> E.Int l (- i) "..."
      E.Frac l r _ -> E.Frac l (- r) "..."
      _ -> error_x "literal_negate" lit

-- | Translate (((f x) y) z) as (f x y z)
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

pat_sch :: Show l => Name_Table -> E.Pat l -> Sch.Exp
pat_sch tbl p =
  case p of
    E.PApp _ (E.Special _ (E.UnitCon _)) [] -> Sch.List []
    E.PVar _ nm -> Sch.Symbol (name_str tbl nm)
    E.PLit _ sgn lit -> literal_sch (if sign_is_negative sgn then literal_negate lit else lit)
    E.PWildCard _ -> Sch.Symbol "_" -- allow singular wildcard
    E.PTuple _ _ _var -> error "pat: tuple let bindings not implemented"
    E.PList _ _var -> error "pat: list let bindings not implemented"
    _ -> error_x "pat" p

-- | An alternative in a case expression.
alt_sch :: Show l => Name_Table -> E.Alt l -> (Sch.Exp,Sch.Exp)
alt_sch tbl alt =
  case alt of
    E.Alt _ (E.PWildCard _) rhs Nothing -> (Sch.Symbol "else",exp_sch tbl (rhs_exp rhs))
    E.Alt _ lhs rhs Nothing -> (Sch.List [pat_sch tbl lhs],exp_sch tbl (rhs_exp rhs))
    _ -> error_x "alt: bindings?" alt

stmt_sch :: Show l => Name_Table -> E.Stmt l -> Sch.Exp
stmt_sch tbl stmt =
  case stmt of
    E.Generator _ p e -> Sch.Set (pat_sch tbl p) (exp_sch tbl e)
    E.Qualifier _ e -> exp_sch tbl e
    _ -> error_x "stmt_sch: not exp?" stmt

exp_is_unit :: E.Exp l -> Bool
exp_is_unit e =
  case e of
    E.Con _ (E.Special _ (E.UnitCon _)) -> True
    _ -> False

exp_sch :: Show l => Name_Table -> E.Exp l -> Sch.Exp
exp_sch tbl e =
  case e of
    E.App _ f x ->
      if exp_is_unit x
      then Sch.App (exp_sch tbl f) []
      else case unwind_app (f,x) of
             fn:arg -> Sch.App (exp_sch tbl fn) (map (exp_sch tbl) arg)
             _ -> error "exp: app"
    E.Case _ c a -> Sch.Case (exp_sch tbl c) (map (alt_sch tbl) a)
    E.Con _ nm -> Sch.Symbol (qname_str tbl nm) -- ?
    E.Do _ st -> Sch.Begin (map (stmt_sch tbl) st)
    E.EnumFromTo _ p q -> Sch.App (Sch.Symbol "enumFromTo") (map (exp_sch tbl) [p,q])
    E.EnumFromThenTo _ p q r -> Sch.App (Sch.Symbol "enumFromThenTo") (map (exp_sch tbl) [p,q,r])
    E.ExpTypeSig _ e' _ -> exp_sch tbl e' -- discard type annotation
    E.If _ p q r -> Sch.If (exp_sch tbl p) (exp_sch tbl q) (exp_sch tbl r)
    E.InfixApp _ lhs qop rhs -> Sch.App (Sch.Symbol (qop_str tbl qop)) [exp_sch tbl lhs,exp_sch tbl rhs]
    E.Lambda _ p c ->
      let arg = case p of
                  [E.PApp _ (E.Special _ (E.UnitCon _)) []] -> []
                  _ -> map (pat_sch tbl) p
      in Sch.Lambda arg (exp_sch tbl c)
    E.LeftSection _ p q ->
      let nm = Sch.Symbol "_leftSectionArg"
      in Sch.Lambda [nm] (Sch.App (Sch.Symbol (qop_str tbl q)) [exp_sch tbl p, nm])
    E.Let _ (E.BDecls _ d) e' -> Sch.Let (map (decl_sch tbl) d) (exp_sch tbl e')
    E.List _ l -> Sch.List (map (exp_sch tbl) l)
    E.Lit _ l -> literal_sch l
    E.NegApp _ n ->
      case n of
        E.Lit _ l -> literal_sch (literal_negate l)
        _ -> Sch.App (Sch.Symbol "negate") [exp_sch tbl n] -- ?

    E.Paren _ e' -> exp_sch tbl e'
    E.RightSection _ p q ->
      let nm = Sch.Symbol "_rightSectionArg"
      in Sch.Lambda [nm] (Sch.App (Sch.Symbol (qop_str tbl p)) [nm,exp_sch tbl q])
    E.Tuple _ _ l -> Sch.Tuple (map (exp_sch tbl) l)
    E.Var _ nm -> Sch.Symbol (qname_str tbl nm)
    _ -> error_x "exp_sch: unimplemented expression type" e

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
      E.PApp _ _ _ -> error_x "pat_var_str: PApp not PVar" p
      _ -> error_x "pat_var_str" p

-- | Clause of a function binding.
match_sch :: Show l => Name_Table -> E.Match l -> (Sch.Exp, Sch.Exp)
match_sch tbl m =
  case m of
    E.Match _ nm param rhs Nothing ->
      let nm' = Sch.Symbol (name_str tbl nm)
          param' = case param of
                     [E.PApp _ (E.Special _ (E.UnitCon _)) []] -> []
                     _ -> map (pat_sch tbl) param
          rhs' = exp_sch tbl (rhs_exp rhs)
      in (nm',Sch.Lambda param' rhs')
    _ -> error_x "match: infix function definitons not allowed" m

decl_sch :: Show l => Name_Table -> E.Decl l -> (Sch.Exp, Sch.Exp)
decl_sch tbl d =
  case d of
    E.FunBind _ [m] -> let (nm,rhs) = match_sch tbl m in (nm, rhs)
    E.PatBind _ lhs rhs bnd ->
      case binds_decl bnd of
        [] -> (pat_sch tbl lhs,exp_sch tbl (rhs_exp rhs))
        _ -> error_x "decl" bnd
    _ -> error_x "decl" d

mod_decl_sch :: Show l => Name_Table -> E.Decl l -> Maybe Sch.Exp
mod_decl_sch tbl d =
  case d of
    E.FunBind _ [m] ->
      let (nm,rhs) = match_sch tbl m
      in Just (Sch.Define nm rhs)
    E.PatBind _ lhs rhs bnd ->
      case binds_decl bnd of
        [] -> case pat_var_str tbl lhs of
                "main" -> Just (exp_sch tbl (rhs_exp rhs))
                nm -> Just (Sch.Define (Sch.Symbol nm) (exp_sch tbl (rhs_exp rhs)))
        _ -> error_x "mod_decl" bnd
    E.TypeSig _ _ _ -> Nothing -- ignore type signatures
    _ -> error_x "mod_decl" d

module_decl :: Show l => E.Module l -> [E.Decl l]
module_decl m =
    case m of
      E.Module _ _ _ _ d -> d
      _ -> error_x "mod_decl" m

hs_exp_sch :: Name_Table -> String -> Sch.Exp
hs_exp_sch tbl s =
  case E.parseExp s of
    E.ParseOk e -> exp_sch tbl e
    err -> error_x "hs_exp" err

{- | Haskell expression to s-expression.

> let rw = hs_exp_to_lisp []
> rw "()" == "unit"
> rw "f ()" == "(f)"
> rw "f x" == "(f x)"
> rw "f x y" == "(f x y)"
> rw "x + y" == "(+ x y)"
> rw "let x = y in x" == "(let ((x y)) x)"
> rw "let {x = i;y = j} in x + y" == "(let* ((x i) (y j)) (+ x y))"
> rw "\\() -> x ()" == "(lambda () (x))"
> rw "\\x -> x * x" == "(lambda (x) (* x x))"
> rw "\\x y -> x * x + y * y" == "(lambda (x y) (+ (* x x) (* y y)))"
> rw "[]" == "(quote ())"
> rw "[1,2,3]" == "(list 1 2 3)"
> rw "(1,2.0,'3',\"4\")" == "(vector 1 2.0 #\\3 \"4\")"
> rw "[x .. y]" == "(enumFromTo x y)"
> rw "[x,y .. z]" == "(enumFromThenTo x y z)"
> rw "if x then y else z" == "(if x y z)"
> rw "\\x -> case x of {0 -> 5;1 -> 4;_ -> 3}" == "(lambda (x) (case x ((0) 5) ((1) 4) (else 3)))"
> rw "(+ 1)" == "(lambda (_rightSectionArg) (+ _rightSectionArg 1))"
> rw "(1 +)" == "(lambda (_leftSectionArg) (+ 1 _leftSectionArg))"
> rw "do {display 0;display (quote x)}" == "(begin (display 0) (display (quote x)))"
> rw "let x = 5 in do {x <- quote five;display x}" == "(let ((x 5)) (begin (set! x (quote five)) (display x)))"
> rw "display 5 >> display (quote five)" == "(>> (display 5) (display (quote five)))"
> rw "let f x = x * 2 in f 3" == "(let ((f (lambda (x) (* x 2)))) (f 3))"
> rw "let f () = act () in f ()" == "(let ((f (lambda () (act)))) (f))"
> rw "let (i,j,k) = (1,2,3) in (k,j,i)" == undefined
> rw "let [i,j,k] = [1,2,3] in (k,j,i)" == undefined
> rw "[(x,y) | x <- [1,2,3], y <- \"abc\"]" == undefined
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
hs_decl_sch :: Name_Table -> String -> Sch.Exp
hs_decl_sch tbl s =
  case E.parseDecl s of
    E.ParseOk d -> fromMaybe (error "hs_decl") (mod_decl_sch tbl d)
    err -> error_x "hs_decl" err

hs_module_sch :: Name_Table -> String -> [Sch.Exp]
hs_module_sch tbl s =
    case E.parseModule s of
      E.ParseOk m -> mapMaybe (mod_decl_sch tbl) (module_decl m)
      err -> error_x "hs_module" err

{- | Translate haskell @module@ code into @LISP@.

> let rw = hs_to_lisp []
> rw "import Sound.SC3" == ""
> rw "o = let f = midiCps (mce [65.0,65.1]) in sinOsc ar f 0" == "(define o (let ((f (midiCps (mce (65.0 65.1))))) (sinOsc ar f 0)))\n"
> rw "a = dbAmp (-24)" == "(define a (dbAmp -24))\n"
> rw "main = audition (out 0 (o * a))" == "(audition (out 0 (* o a)))\n"
> rw "x = (1,2,3)" == "(define x (vector 1 2 3))\n"
> rw "sq n = n * n" == "(define sq (lambda (n) (* n n)))\n"
> rw "abs n = if n >= 0 then n else -n" == "(define abs (lambda (n) (if (>= n 0) n (negate n))))\n"
> rw "l = [0 .. 9]" == "(define l (enumFromTo 0 9))\n"
> rw "l = [0, 2 .. 8]" == "(define l (enumFromThenTo 0 2 8))\n"
> rw "n = 0.1 * 0.01 * 0.001" == "(define n (* (* 0.1 0.01) 0.001))\n"
> rw "l = [1,1.0,\"str\",'c']" == "(define l (1 1.0 \"str\" #\\c))\n"
> rw "sq = \\x -> x * x" == "(define sq (lambda (x) (* x x)))\n"
> rw "sum_sq = \\x y -> x * x + y * y" == "(define sum_sq (lambda (x y) (+ (* x x) (* y y))))\n"
> rw "l = 1 : []" == "(define l (cons 1 (quote ())))\n"
> rw "main = putStrLn \"text\"" == "(putStrLn \"text\")\n"
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

-- * Sch to SExp

sch_unassoc :: [(Sch.Exp, Sch.Exp)] -> [L.SExp]
sch_unassoc = map (\(lhs,rhs) -> S.List [sch_to_sexp lhs, sch_to_sexp rhs])

sch_to_sexp :: Sch.Exp -> L.SExp
sch_to_sexp e =
  case e of
    Sch.Char c -> S.Char c
    Sch.String s -> S.String s
    Sch.Integer i -> S.Number i
    Sch.Double d -> S.Float d
    Sch.Symbol s -> S.Atom s
    Sch.List l -> if null l then S.List [S.Atom "quote",S.Atom "()"] else S.List (S.Atom "list" : map sch_to_sexp l)
    Sch.Case c a -> S.List (S.Atom "case" : sch_to_sexp c : sch_unassoc a)
    Sch.Set lhs rhs -> S.List [S.Atom "set!", sch_to_sexp lhs, sch_to_sexp rhs]
    Sch.App f a -> S.List (sch_to_sexp f : map sch_to_sexp a)
    Sch.Begin x -> S.List (S.Atom "begin" : map sch_to_sexp x)
    Sch.If p q r -> S.List (S.Atom "if" : map sch_to_sexp [p,q,r])
    Sch.Lambda p x -> S.List [S.Atom "lambda", S.List (map sch_to_sexp p), sch_to_sexp x]
    Sch.Let d x -> S.List [S.Atom (if length d == 1 then "let" else "let*"), S.List (sch_unassoc d), sch_to_sexp x]
    Sch.Tuple t -> S.List (S.Atom "vector" : map sch_to_sexp t)
    Sch.Define lhs rhs -> S.List [S.Atom "define", sch_to_sexp lhs, sch_to_sexp rhs]

hs_exp_sexp :: Name_Table -> String -> L.SExp
hs_exp_sexp tbl = sch_to_sexp . hs_exp_sch tbl

hs_decl_sexp :: Name_Table -> String -> L.SExp
hs_decl_sexp tbl = sch_to_sexp . hs_decl_sch tbl

hs_module_sexp :: Name_Table -> String -> [L.SExp]
hs_module_sexp tbl = map sch_to_sexp . hs_module_sch tbl

{-
-- | Tuples map to vectors.
exp_sexp :: Show l => Name_Table -> E.Exp l -> L.SExp
exp_sexp tbl e =
    case e of
      E.App _ f x -> if exp_is_unit x
                     then S.List [exp_sexp tbl f]
                     else S.List (map (exp_sexp tbl) (unwind_app (f,x)))
      E.Case _ c a -> S.List (S.Atom "case" : exp_sexp tbl c : map (alt_sexp tbl) a)
      E.Con _ nm -> S.Atom (qname_str tbl nm) -- ?
      E.Do _ st -> S.List (S.Atom "begin" : map (stmt_sexp tbl) st)
      E.EnumFromTo _ p q -> S.List (S.Atom "enumFromTo" : map (exp_sexp tbl) [p,q])
      E.EnumFromThenTo _ p q r -> S.List (S.Atom "enumFromThenTo" : map (exp_sexp tbl) [p,q,r])
      E.ExpTypeSig _ e' _ -> exp_sexp tbl e' -- discard type annotation
      E.If _ p q r -> S.List (S.Atom "if" : map (exp_sexp tbl) [p,q,r])
      E.InfixApp _ lhs qop rhs -> S.List [S.Atom (qop_str tbl qop),exp_sexp tbl lhs,exp_sexp tbl rhs]
      E.Lambda _ p c ->
        let arg = case p of
                    [E.PApp _ (E.Special _ (E.UnitCon _)) []] -> S.List []
                    _ -> S.List (map (pat_sexp tbl) p)
        in S.List [S.Atom "lambda",arg,exp_sexp tbl c]
      E.LeftSection _ p q ->
          let nm = S.Atom "_leftSectionArg"
          in S.List [S.Atom "lambda",S.List [nm],S.List [S.Atom (qop_str tbl q),exp_sexp tbl p,nm]]
      E.Let _ (E.BDecls _ d) e' -> S.List [S.Atom (if length d == 1 then "let" else "let*")
                                          ,S.List (map (decl_sexp tbl) d)
                                          ,exp_sexp tbl e']
      E.List _ l -> if null l
                    then S.List [S.Atom "quote",S.Atom "()"]
                    else S.List (S.Atom "list" : map (exp_sexp tbl) l)
      E.Lit _ l -> literal_sexp l
      E.NegApp _ n ->
          case n of
            E.Lit _ l -> literal_sexp (literal_negate l)
            _ -> S.List [S.Atom "negate",exp_sexp tbl n]
      E.Paren _ e' -> exp_sexp tbl e'
      E.RightSection _ p q ->
          let nm = S.Atom "_rightSectionArg"
          in S.List [S.Atom "lambda",S.List [nm],S.List [S.Atom (qop_str tbl p),nm,exp_sexp tbl q]]
      E.Tuple _ _ l -> S.List (S.Atom "vector" : map (exp_sexp tbl) l)
      E.Var _ nm -> S.Atom (qname_str tbl nm)
      _ -> error_x "exp_sexp: unimplemented expression type" e

-- | Top-level declaration.
decl_sexp :: Show l => Name_Table -> E.Decl l -> L.SExp
decl_sexp tbl d =
    case d of
      E.FunBind _ [m] -> let (nm,rhs) = match_sexp tbl m in S.List [nm,rhs]
      E.PatBind _ lhs rhs bnd ->
          case binds_decl bnd of
            [] -> S.List [pat_sexp tbl lhs,exp_sexp tbl (rhs_exp rhs)]
            _ -> error_x "decl_sexp" bnd
      _ -> error_x "decl_sexp" d

-- | Clause of a function binding.
match_sexp :: Show l => Name_Table -> E.Match l -> (L.SExp,L.SExp)
match_sexp tbl m =
    case m of
      E.Match _ nm param rhs Nothing ->
          let nm' = S.Atom (name_str tbl nm)
              param' = case param of
                         [E.PApp _ (E.Special _ (E.UnitCon _)) []] -> []
                         _ -> map (pat_sexp tbl) param
              rhs' = exp_sexp tbl (rhs_exp rhs)
          in (nm',S.List [S.Atom "lambda",S.List param',rhs'])
      _ -> error_x "match_sexp: infix function definitons not allowed" m

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
      E.TypeSig _ _ _ -> Nothing -- ignore type signatures
      _ -> error_x "mod_decl_sexp" d

literal_sexp :: Show l => E.Literal l -> L.SExp
literal_sexp l =
    case l of
      E.Char _ c _ -> S.Char c
      E.String _ s _ -> S.String s
      E.Int _ i _ -> S.Number i
      E.Frac _ r _ -> S.Float (fromRational r)
      _ -> error_x "literal_sexp" l

pat_sexp :: Show l => Name_Table -> E.Pat l -> L.SExp
pat_sexp tbl p =
    case p of
      E.PApp _ (E.Special _ (E.UnitCon _)) [] -> S.List []
      E.PVar _ nm -> S.Atom (name_str tbl nm)
      E.PLit _ sgn lit -> literal_sexp (if sign_is_negative sgn then literal_negate lit else lit)
      E.PWildCard _ -> S.Atom "_" -- allow singular wildcard
      E.PTuple _ _ _var -> error "pat_sexp: tuple let bindings not implemented"
      _ -> error_x "pat_sexp" p

-- | An alternative in a case expression.
alt_sexp :: Show l => Name_Table -> E.Alt l -> L.SExp
alt_sexp tbl alt =
  case alt of
    E.Alt _ (E.PWildCard _) rhs Nothing -> S.List [S.Atom "else",exp_sexp tbl (rhs_exp rhs)]
    E.Alt _ lhs rhs Nothing -> S.List [S.List [pat_sexp tbl lhs],exp_sexp tbl (rhs_exp rhs)]
    _ -> error_x "alt_sexp: bindings?" alt

stmt_sexp :: Show l => Name_Table -> E.Stmt l -> L.SExp
stmt_sexp tbl stmt =
  case stmt of
    E.Generator _ p e -> S.List [S.Atom "set!",pat_sexp tbl p,exp_sexp tbl e]
    E.Qualifier _ e -> exp_sexp tbl e
    _ -> error_x "stmt_sexp: not exp?" stmt

-}

