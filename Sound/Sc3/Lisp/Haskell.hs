-- | Rewrite a subset of Haskell (.sch) as Lisp.
module Sound.Sc3.Lisp.Haskell where

import Data.Maybe {- base -}

import qualified Language.Haskell.Exts as E {- haskell-src-exts -}

import qualified Language.Scheme.Types as S {- husk-scheme -}

import qualified Sound.Sc3.Lisp.NameTable as Tbl {- hsc3-lisp -}
import qualified Sound.Sc3.Lisp.Parse.Ethier as L {- hsc3-lisp -}
import qualified Sound.Sc3.Lisp.Sch as Sch {- hsc3-lisp -}

-- | Format error string.
error_x :: Show a => String -> a -> t
error_x nm x = error (nm ++ ": " ++ show x)

-- | Names are re-written using a lookup table.
name_str :: E.Name l -> String
name_str nm =
  case nm of
    E.Symbol _ s -> s
    E.Ident _ s -> s

-- | Unqualified QName to String, cons and unit are special cases.
qname_str :: Show l => E.QName l -> String
qname_str q =
  case q of
    E.UnQual _ nm -> name_str nm
    E.Special _ (E.Cons _) -> "cons"
    E.Special _ (E.UnitCon _) -> "unit"
    _ -> error_x "qname_str" q

-- | Unqualified QOp to String.
qop_str :: Show l => E.QOp l -> String
qop_str q =
  case q of
    E.QVarOp _ nm -> qname_str nm
    E.QConOp _ nm -> qname_str nm

-- | Rhs to Exp, only unguarded values are allowed.
rhs_exp :: Show l => E.Rhs l -> E.Exp l
rhs_exp r =
  case r of
    E.UnGuardedRhs _ e -> e
    _ -> error_x "rhs_exp: not unguarded" r

-- | Literal to Exp.
literal_sch :: Show l => E.Literal l -> Sch.Exp
literal_sch l =
  case l of
    E.Char _ c _ -> Sch.Char c
    E.String _ s _ -> Sch.String s
    E.Int _ i _ -> Sch.Integer i
    E.Frac _ r _ -> Sch.Double (fromRational r)
    _ -> error_x "literal: unboxed literals not allowed" l

-- | Negate Literal.
literal_negate :: Show l => E.Literal l -> E.Literal l
literal_negate lit =
  case lit of
    E.Int l i _ -> E.Int l (-i) "..."
    E.Frac l r _ -> E.Frac l (-r) "..."
    _ -> error_x "literal_negate: not a numeric literal" lit

-- | Translate (((f x) y) z) as (f x y z)
unwind_app :: (E.Exp l, E.Exp l) -> [E.Exp l]
unwind_app (lhs, rhs) =
  case lhs of
    E.App _ p q -> unwind_app (p, q) ++ [rhs]
    _ -> [lhs, rhs]

-- | Is Sign negative.
sign_is_negative :: E.Sign l -> Bool
sign_is_negative sgn =
  case sgn of
    E.Signless _ -> False
    E.Negative _ -> True

-- | A pattern, to be matched against a value.
pat_sch :: Show l => E.Pat l -> Sch.Exp
pat_sch p =
  case p of
    E.PApp _ (E.Special _ (E.UnitCon _)) [] -> Sch.List []
    E.PVar _ nm -> Sch.Symbol (name_str nm)
    E.PLit _ sgn lit -> literal_sch (if sign_is_negative sgn then literal_negate lit else lit)
    E.PWildCard _ -> Sch.Symbol "_" -- allow singular wildcard
    E.PTuple _ _ e -> Sch.Tuple (map pat_sch e)
    E.PList _ e -> Sch.List (map pat_sch e)
    _ -> error_x "pat" p

-- | Get Var name from Pat else error.
pat_var_str :: Show l => E.Pat l -> String
pat_var_str p =
  case p of
    E.PVar _ nm -> name_str nm
    E.PApp _ _ _ -> error_x "pat_var_str: PApp not PVar" p
    _ -> error_x "pat_var_str" p

-- | An alternative, in a case expression.
alt_sch :: Show l => E.Alt l -> (Sch.Exp, Sch.Exp)
alt_sch alt =
  case alt of
    E.Alt _ (E.PWildCard _) rhs Nothing -> (Sch.Symbol "else", exp_sch (rhs_exp rhs))
    E.Alt _ lhs rhs Nothing -> (pat_sch lhs, exp_sch (rhs_exp rhs))
    _ -> error_x "alt: bindings?" alt

-- | A statement.
stmt_sch :: Show l => E.Stmt l -> Sch.Exp
stmt_sch stmt =
  case stmt of
    E.Generator _ p e -> Sch.Set (pat_sch p) (exp_sch e)
    E.Qualifier _ e -> exp_sch e
    _ -> error_x "stmt_sch: not exp?" stmt

-- | Is Exp the Unit constructor?
exp_is_con_unit :: E.Exp l -> Bool
exp_is_con_unit e =
  case e of
    E.Con _ (E.Special _ (E.UnitCon _)) -> True
    _ -> False

-- | Haskell expression to Sch expression.
exp_sch :: Show l => E.Exp l -> Sch.Exp
exp_sch e =
  case e of
    E.App _ f x ->
      if exp_is_con_unit x
        then Sch.App (exp_sch f) []
        else case unwind_app (f, x) of
          fn : arg -> Sch.App (exp_sch fn) (map (exp_sch) arg)
          _ -> error "exp: app"
    E.Case _ c a -> Sch.Case (exp_sch c) (map alt_sch a)
    E.Con _ nm -> Sch.Symbol (qname_str nm) -- ?
    E.Do _ st -> Sch.Begin (map stmt_sch st)
    E.EnumFromTo _ p q -> Sch.App (Sch.Symbol "enumFromTo") (map exp_sch [p, q])
    E.EnumFromThenTo _ p q r -> Sch.App (Sch.Symbol "enumFromThenTo") (map exp_sch [p, q, r])
    E.ExpTypeSig _ e' _ -> exp_sch e' -- discard type annotation
    E.If _ p q r -> Sch.If (exp_sch p) (exp_sch q) (exp_sch r)
    E.InfixApp _ lhs qop rhs -> Sch.App (Sch.Symbol (qop_str qop)) [exp_sch lhs, exp_sch rhs]
    E.Lambda _ p c ->
      let arg = case p of
            [E.PApp _ (E.Special _ (E.UnitCon _)) []] -> []
            _ -> map pat_sch p
      in Sch.Lambda arg (exp_sch c)
    E.LeftSection _ p q ->
      let nm = Sch.Symbol "_leftSectionArg"
      in Sch.Lambda [nm] (Sch.App (Sch.Symbol (qop_str q)) [exp_sch p, nm])
    E.Let _ (E.BDecls _ d) e' -> Sch.Let (map decl_sch d) (exp_sch e')
    E.List _ l -> Sch.List (map exp_sch l)
    E.Lit _ l -> literal_sch l
    E.NegApp _ n ->
      case n of
        E.Lit _ l -> literal_sch (literal_negate l)
        _ -> Sch.App (Sch.Symbol "negate") [exp_sch n] -- ?
    E.Paren _ e' -> exp_sch e'
    E.RightSection _ p q ->
      let nm = Sch.Symbol "_rightSectionArg"
      in Sch.Lambda [nm] (Sch.App (Sch.Symbol (qop_str p)) [nm, exp_sch q])
    E.Tuple _ _ l -> Sch.Tuple (map exp_sch l)
    E.Var _ nm -> Sch.Symbol (qname_str nm)
    _ -> error_x "exp_sch: unimplemented expression type" e

-- | Get Decl from a binding group inside a let or where clause.
binds_decl :: Show l => Maybe (E.Binds l) -> [E.Decl l]
binds_decl b =
  case b of
    Nothing -> []
    Just (E.BDecls _ d) -> d
    Just (E.IPBinds _ _) -> error_x "binds_decl: implicit parameters?" b

-- | Clause of a function binding.
match_sch :: Show l => E.Match l -> (Sch.Exp, Sch.Exp)
match_sch m =
  case m of
    E.Match _ nm param rhs Nothing ->
      let nm' = Sch.Symbol (name_str nm)
          param' = case param of
            [E.PApp _ (E.Special _ (E.UnitCon _)) []] -> []
            _ -> map pat_sch param
          rhs' = exp_sch (rhs_exp rhs)
      in (nm', Sch.Lambda param' rhs')
    _ -> error_x "match: infix function definitons not allowed" m

-- | Declaration as left and right hand side expressions.
decl_sch :: Show l => E.Decl l -> (Sch.Exp, Sch.Exp)
decl_sch d =
  case d of
    E.FunBind _ [m] -> let (nm, rhs) = match_sch m in (nm, rhs)
    E.PatBind _ lhs rhs bnd ->
      case binds_decl bnd of
        [] -> (pat_sch lhs, exp_sch (rhs_exp rhs))
        _ -> error_x "decl" bnd
    _ -> error_x "decl" d

-- | Module declaration, either as Define form or if main as Exp.
mod_decl_sch :: Show l => E.Decl l -> Maybe Sch.Exp
mod_decl_sch d =
  case d of
    E.FunBind _ [m] ->
      let (nm, rhs) = match_sch m
      in Just (Sch.Define nm rhs)
    E.PatBind _ lhs rhs bnd ->
      case binds_decl bnd of
        [] -> case pat_var_str lhs of
          "main" -> Just (exp_sch (rhs_exp rhs))
          nm -> Just (Sch.Define (Sch.Symbol nm) (exp_sch (rhs_exp rhs)))
        _ -> error_x "mod_decl" bnd
    E.TypeSig _ _ _ -> Nothing -- ignore type signatures
    _ -> error_x "mod_decl" d

-- | List of Decl at Module.
module_decl :: Show l => E.Module l -> [E.Decl l]
module_decl m =
  case m of
    E.Module _ _ _ _ d -> d
    _ -> error_x "mod_decl: not ordinary module" m

-- | Parse Haskell expression as Exp, i.e. exp_sch of parseExp.
hs_exp_sch :: String -> Sch.Exp
hs_exp_sch s =
  case E.parseExp s of
    E.ParseOk e -> exp_sch e
    err -> error_x "hs_exp" err

{- | Parse Haskell declaration as Exp, i.e. mod_decl_sch of parseDecl.

>>> let rw = L.sexp_show . hs_decl_lisp []
>>> rw "x = y"
"(define x y)"

>>> rw "f x = case x of {0 -> a;1 -> b;_ -> c}"
"(define f (lambda (x) (case x ((0) a) ((1) b) (else c))))"

>>> rw "x = y"
"(define x y)"

>>> rw "f x = x * x"
"(define f (lambda (x) (* x x)))"

>>> rw "main = x"
"x"
-}
hs_decl_sch :: String -> Sch.Exp
hs_decl_sch s =
  case E.parseDecl s of
    E.ParseOk d -> fromMaybe (error "hs_decl") (mod_decl_sch d)
    err -> error_x "hs_decl" err

-- | Parse Haskell module as [Exp], i.e. map mod_decl_sch of parseModule.
hs_module_sch :: String -> [Sch.Exp]
hs_module_sch s =
  case E.parseModule s of
    E.ParseOk m -> mapMaybe mod_decl_sch (module_decl m)
    err -> error_x "hs_module" err

-- * Strings

{- | Haskell expression to s-expression.

>>> let rw = hs_exp_to_lisp []
>>> rw "()"
"unit"

>>> rw "f ()"
"(f)"

>>> rw "f x"
"(f x)"

>>> rw "f x y"
"(f x y)"

>>> rw "x + y"
"(+ x y)"

>>> rw "let x = y in x"
"(letrec ((x y)) x)"

>>> rw "let {x = i;y = j} in x + y"
"(letrec ((x i) (y j)) (+ x y))"

>>> rw "\\() -> x ()"
"(lambda () (x))"

>>> rw "\\x -> x * x"
"(lambda (x) (* x x))"

>>> rw "\\x y -> x * x + y * y"
"(lambda (x y) (+ (* x x) (* y y)))"

>>> rw "\\(p, q) r -> p + q * r"
"(lambda (_p1 _p2) (let* ((_letPatBind _p1) (p (vectorRef _letPatBind 0)) (q (vectorRef _letPatBind 1))) (letrec ((r _p2)) (+ p (* q r)))))"

>>> rw "[]"
"(quote ())"

>>> rw "[1,2,3]"
"(list 1 2 3)"

>>> rw "(1,2.0,'3',\"4\")"
"(vector 1 2.0 #\\3 \"4\")"

>>> rw "[x .. y]"
"(enumFromTo x y)"

>>> rw "[x,y .. z]"
"(enumFromThenTo x y z)"

>>> rw "if x then y else z"
"(if x y z)"

>>> rw "\\x -> case x of {0 -> 5;1 -> 4;_ -> 3}"
"(lambda (x) (case x ((0) 5) ((1) 4) (else 3)))"

>>> rw "(+ 1)"
"(lambda (_rightSectionArg) (+ _rightSectionArg 1))"

>>> rw "(1 +)"
"(lambda (_leftSectionArg) (+ 1 _leftSectionArg))"

>>> rw "do {display 0;display (quote x)}"
"(begin (display 0) (display (quote x)))"

>>> rw "let x = 5 in do {x <- quote five;display x}"
"(letrec ((x 5)) (begin (set! x (quote five)) (display x)))"

>>> rw "display 5 >> display (quote five)"
"(>> (display 5) (display (quote five)))"

>>> rw "let f x = x * 2 in f 3"
"(letrec ((f (lambda (x) (* x 2)))) (f 3))"

>>> rw "let f () = act () in f ()"
"(letrec ((f (lambda () (act)))) (f))"

>>> rw "let f (p, q) = p + q in f (2, 3)"
"(letrec ((f (lambda (_p1) (let* ((_letPatBind _p1) (p (vectorRef _letPatBind 0)) (q (vectorRef _letPatBind 1))) (+ p q))))) (f (vector 2 3)))"

>>> rw "let (i,j) = (1,2) in (j,i)"
"(let* ((_letPatBind (vector 1 2)) (i (vectorRef _letPatBind 0)) (j (vectorRef _letPatBind 1))) (vector j i))"

>>> rw "let [i,j] = [1,2] in (j,i)"
"(let* ((_letPatBind (list 1 2)) (i (listRef _letPatBind 0)) (j (listRef _letPatBind 1))) (vector j i))"

> rw "[(x,y) | x <- [1,2,3], y <- \"abc\"]"
undefined

>>> hs_exp_to_lisp [("+","add")] "1 + 2"
"(add 1 2)"
-}
hs_exp_to_lisp :: Tbl.NameTable -> String -> String
hs_exp_to_lisp tbl = L.sexp_show . hs_exp_lisp tbl

{- | Translate haskell @module@ code into @Lisp@.

>>> let rw = hs_to_lisp []
>>> rw "import Sound.Sc3"
""

>>> rw "o = let f = midiCps (mce [65.0,65.1]) in sinOsc ar f 0"
"(define o (letrec ((f (midiCps (mce (list 65.0 65.1))))) (sinOsc ar f 0)))\n"

>>> rw "a = dbAmp (-24)"
"(define a (dbAmp -24))\n"

>>> rw "main = audition (out 0 (o * a))"
"(audition (out 0 (* o a)))\n"

>>> rw "x = (1,2,3)"
"(define x (vector 1 2 3))\n"

>>> rw "sq n = n * n"
"(define sq (lambda (n) (* n n)))\n"

>>> rw "abs n = if n >= 0 then n else -n"
"(define abs (lambda (n) (if (>= n 0) n (negate n))))\n"

>>> rw "l = [0 .. 9]"
"(define l (enumFromTo 0 9))\n"

>>> rw "l = [0, 2 .. 8]"
"(define l (enumFromThenTo 0 2 8))\n"

>>> rw "n = 0.1 * 0.01 * 0.001"
"(define n (* (* 0.1 0.01) 0.001))\n"

>>> rw "l = [1,1.0,\"str\",'c']"
"(define l (list 1 1.0 \"str\" #\\c))\n"

>>> rw "sq = \\x -> x * x"
"(define sq (lambda (x) (* x x)))\n"

>>> rw "sum_sq = \\x y -> x * x + y * y"
"(define sum_sq (lambda (x y) (+ (* x x) (* y y))))\n"

>>> rw "l = 1 : []"
"(define l (cons 1 (quote ())))\n"

>>> rw "main = putStrLn \"text\""
"(putStrLn \"text\")\n"
-}
hs_to_lisp :: Tbl.NameTable -> String -> String
hs_to_lisp tbl = unlines . map L.sexp_show . hs_module_lisp tbl

-- * Io

hs_to_lisp_f_io :: (Tbl.NameTable -> String -> String) -> Maybe FilePath -> FilePath -> FilePath -> IO ()
hs_to_lisp_f_io proc_f tbl_fn i_fn o_fn = do
  tbl <- maybe (return []) Tbl.nameTableLoad tbl_fn
  i <- readFile i_fn
  writeFile o_fn (proc_f tbl i)

hs_to_lisp_io :: Maybe FilePath -> FilePath -> FilePath -> IO ()
hs_to_lisp_io = hs_to_lisp_f_io hs_to_lisp

hs_exp_to_lisp_io :: Maybe FilePath -> FilePath -> FilePath -> IO ()
hs_exp_to_lisp_io = hs_to_lisp_f_io hs_exp_to_lisp

-- * Sch to Lisp

sch_case_to_lisp :: Sch.Exp -> [(Sch.Exp, Sch.Exp)] -> S.LispVal
sch_case_to_lisp c a =
  let rw_lhs lhs = if Sch.is_symbol lhs then sch_to_lisp lhs else S.List [sch_to_lisp lhs]
      rw (lhs, rhs) = S.List [rw_lhs lhs, sch_to_lisp rhs]
  in S.List (S.Atom "case" : sch_to_lisp c : map rw a)

sch_let_pat_to_lisp :: String -> [Sch.Exp] -> Sch.Exp -> S.LispVal
sch_let_pat_to_lisp ref lhs rhs =
  let bnd = S.Atom "_letPatBind"
      outer = S.List [bnd, sch_to_lisp rhs]
      inner var ix = S.List [sch_to_lisp var, S.List [S.Atom ref, bnd, S.Number ix]]
  in S.List (outer : zipWith inner lhs [0 ..])

-- | Special case where all bindings are simple (non-pattern).
sch_let_to_lisp :: [(Sch.Exp, Sch.Exp)] -> Sch.Exp -> S.LispVal
sch_let_to_lisp d x =
  if null d
    then sch_to_lisp x
    else
      if all (Sch.is_symbol . fst) d
        then
          S.List
            [ S.Atom "letrec" -- (if length d == 1 then "let" else "let*")
            , S.List (map (\(lhs, rhs) -> S.List [sch_to_lisp lhs, sch_to_lisp rhs]) d)
            , sch_to_lisp x
            ]
        else sch_let_to_lisp_pat d x

-- | Pattern bindings introduce nested let sequences, recur back to general case.
sch_let_to_lisp_pat :: [(Sch.Exp, Sch.Exp)] -> Sch.Exp -> S.LispVal
sch_let_to_lisp_pat d x =
  case d of
    [] -> sch_to_lisp x
    (lhs, rhs) : d' ->
      case lhs of
        Sch.Symbol _ -> S.List [S.Atom "let", S.List [S.List [sch_to_lisp lhs, sch_to_lisp rhs]], sch_let_to_lisp d' x]
        Sch.Tuple var -> S.List [S.Atom "let*", sch_let_pat_to_lisp "vectorRef" var rhs, sch_let_to_lisp d' x]
        Sch.List var -> S.List [S.Atom "let*", sch_let_pat_to_lisp "listRef" var rhs, sch_let_to_lisp d' x]
        _ -> error "sch_let_to_lisp"

-- | Rewriter for case where Lambda parameters are not all symbols.
sch_lambda_rw :: [Sch.Exp] -> Sch.Exp -> Sch.Exp
sch_lambda_rw p x =
  let k = length p
      p' = map (\ix -> Sch.Symbol ("_p" ++ show ix)) [1 .. k]
  in Sch.Lambda p' (Sch.Let (zip p p') x)

sch_to_lisp :: Sch.Exp -> S.LispVal
sch_to_lisp e =
  case e of
    Sch.Char c -> S.Char c
    Sch.String s -> S.String s
    Sch.Integer i -> S.Number i
    Sch.Double d -> S.Float d
    Sch.Symbol s -> S.Atom s
    Sch.List l -> if null l then S.List [S.Atom "quote", S.nullLisp] else S.List (S.Atom "list" : map sch_to_lisp l)
    Sch.Case c a -> sch_case_to_lisp c a
    Sch.Set lhs rhs -> S.List [S.Atom "set!", sch_to_lisp lhs, sch_to_lisp rhs]
    Sch.App f a -> S.List (sch_to_lisp f : map sch_to_lisp a)
    Sch.Begin x -> S.List (S.Atom "begin" : map sch_to_lisp x)
    Sch.If p q r -> S.List (S.Atom "if" : map sch_to_lisp [p, q, r])
    Sch.Lambda p x ->
      if all Sch.is_symbol p
        then S.List [S.Atom "lambda", S.List (map sch_to_lisp p), sch_to_lisp x]
        else sch_to_lisp (sch_lambda_rw p x)
    Sch.Let d x -> sch_let_to_lisp d x
    Sch.Tuple t -> S.List (S.Atom "vector" : map sch_to_lisp t)
    Sch.Define lhs rhs -> S.List [S.Atom "define", sch_to_lisp lhs, sch_to_lisp rhs]

hs_exp_lisp :: Tbl.NameTable -> String -> S.LispVal
hs_exp_lisp tbl = sch_to_lisp . Sch.exp_rename tbl . hs_exp_sch

hs_decl_lisp :: Tbl.NameTable -> String -> S.LispVal
hs_decl_lisp tbl = sch_to_lisp . Sch.exp_rename tbl . hs_decl_sch

hs_module_lisp :: Tbl.NameTable -> String -> [S.LispVal]
hs_module_lisp tbl = map (sch_to_lisp . Sch.exp_rename tbl) . hs_module_sch
