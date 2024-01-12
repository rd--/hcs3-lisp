-- | Rewrite .stc and .spl as Lisp.
module Language.Sc3.Lisp.Spl where

import Data.Maybe {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Stc.Ast as Stc {- stsc3 -}
import qualified Language.Smalltalk.Stc.Translate as Stc {- stsc3 -}

import qualified Language.Scheme.Types as S {- husk-scheme -}

import qualified Language.Sc3.Lisp.Parse.Ethier as L {- hsc3-lisp -}
import Language.Sc3.Lisp.Scs {- hsc3-lisp -}

stNumber_to_exp :: St.Number -> Exp
stNumber_to_exp n =
  case n of
    St.Int x -> Integer x
    St.Float x -> Double x

stLiteral_to_exp :: St.Literal -> Exp
stLiteral_to_exp l =
  case l of
    St.NumberLiteral x -> stNumber_to_exp x
    St.StringLiteral x -> String x
    St.CharacterLiteral x -> Char x
    St.SymbolLiteral x -> App (Symbol "quote") [Symbol x]
    St.SelectorLiteral x -> Symbol (St.selectorIdentifier x)
    St.ArrayLiteral x -> Array (map (either stLiteral_to_exp Symbol) x)

stcStatements_to_exp :: Stc.StcStatements -> Exp
stcStatements_to_exp x =
  case x of
    Stc.StcStatementsReturn (Stc.StcReturnStatement p) -> App (Symbol "return") [stcExpression_to_exp p]
    Stc.StcStatementsExpression p q ->
      case q of
        Nothing -> stcExpression_to_exp p
        Just q' -> Seq (stcExpression_to_exp p) (stcStatements_to_exp q')

-- | Translated as Lambda with interior Let.  Alternately, Lambda could have a temporaries field.
stcBlockBody_to_exp :: Stc.StcBlockBody -> Exp
stcBlockBody_to_exp (Stc.StcBlockBody arg tmp stm) =
  let binder (p, q) = (p, maybe Nil stcBasicExpression_to_exp q)
      body = Let (maybe [] (concatMap (map binder)) tmp) (maybe Nil stcStatements_to_exp stm)
  in Lambda (map fst (fromMaybe [] arg)) body

stcExpression_to_exp :: Stc.StcExpression -> Exp
stcExpression_to_exp e =
  case e of
    Stc.StcExprAssignment p q -> Set (Symbol p) (stcExpression_to_exp q)
    Stc.StcExprBasic p -> stcBasicExpression_to_exp p

-- | rcv.msg(arg) translates as (msg (rcv : arg)).
stcDotMessage_to_exp :: Exp -> Stc.StcDotMessage -> Exp
stcDotMessage_to_exp rcv (Stc.StcDotMessage msg arg) = App (Symbol msg) (rcv : map stcBasicExpression_to_exp arg)

stcDotMessages_to_exp :: Exp -> [Stc.StcDotMessage] -> Exp
stcDotMessages_to_exp rcv m =
  case m of
    [] -> rcv
    p : q -> stcDotMessages_to_exp (stcDotMessage_to_exp rcv p) q

stcBinaryArgument_to_exp :: Stc.StcBinaryArgument -> Exp
stcBinaryArgument_to_exp (Stc.StcBinaryArgument p q) = stcDotMessages_to_exp (stcPrimary_to_exp p) (fromMaybe [] q)

stcBinaryMessage_to_exp :: Exp -> Stc.StcBinaryMessage -> Exp
stcBinaryMessage_to_exp rcv (Stc.StcBinaryMessage (p, _) q) = App (Symbol p) [rcv, stcBinaryArgument_to_exp q]

stcBinaryMessages_to_exp :: Exp -> [Stc.StcBinaryMessage] -> Exp
stcBinaryMessages_to_exp rcv m =
  case m of
    [] -> rcv
    p : q -> stcBinaryMessages_to_exp (stcBinaryMessage_to_exp rcv p) q

stcMessages_to_exp :: Exp -> Stc.StcMessages -> Exp
stcMessages_to_exp rcv m =
  case m of
    Stc.StcMessagesDot p q -> stcBinaryMessages_to_exp (stcDotMessages_to_exp rcv p) (fromMaybe [] q)
    Stc.StcMessagesBinary p -> stcBinaryMessages_to_exp rcv p

stcBasicExpression_to_exp :: Stc.StcBasicExpression -> Exp
stcBasicExpression_to_exp (Stc.StcBasicExpression p q) =
  case q of
    Nothing -> stcPrimary_to_exp p
    Just m -> stcMessages_to_exp (stcPrimary_to_exp p) m

stcPrimary_to_exp :: Stc.StcPrimary -> Exp
stcPrimary_to_exp x =
  case x of
    Stc.StcPrimaryIdentifier p -> Symbol p
    Stc.StcPrimaryLiteral p -> stLiteral_to_exp p
    Stc.StcPrimaryBlock p -> stcBlockBody_to_exp p
    Stc.StcPrimaryExpression p -> stcExpression_to_exp p
    Stc.StcPrimaryArrayExpression p -> Array (map stcBasicExpression_to_exp p)
    Stc.StcPrimaryDictionaryExpression _ -> error "stcPrimary_to_exp: dictionary..."
    Stc.StcPrimaryImplicitMessageSend p q -> App (Symbol p) (map stcBasicExpression_to_exp q)

-- | Translate as let expression.  Alternately could translate as Seq of Set.
stcInitializerDefinition_to_let_exp :: Stc.StcInitializerDefinition -> Exp
stcInitializerDefinition_to_let_exp (Stc.StcInitializerDefinition _cmt tmp stm) =
  let binder (p, q) = (p, maybe Nil stcBasicExpression_to_exp q)
      tmpSeq = concatMap (map binder) (fromMaybe [] tmp)
      stmExp = maybe Nil stcStatements_to_exp stm
  in Let tmpSeq stmExp

-- | Translate as definition sequence with perhaps a subsequent program.
stcInitializerDefinition_to_exp_seq :: Stc.StcInitializerDefinition -> [Exp]
stcInitializerDefinition_to_exp_seq (Stc.StcInitializerDefinition _cmt tmp stm) =
  let binder (p, q) = Define p (maybe Nil stcBasicExpression_to_exp q)
      tmpSeq = concatMap (map binder) (fromMaybe [] tmp)
      stmExp = maybe [] (return . stcStatements_to_exp) stm
  in tmpSeq ++ stmExp

-- * Lisp

let_to_lisp :: [(Name, Exp)] -> Exp -> S.LispVal
let_to_lisp d x =
  if null d
    then exp_to_lisp x
    else
      S.List
        [ S.Atom (if length d == 1 then "let" else "let*") -- "letrec"
        , S.List (map (\(lhs, rhs) -> S.List [S.Atom lhs, exp_to_lisp rhs]) d)
        , exp_to_lisp x
        ]

exp_to_lisp :: Exp -> S.LispVal
exp_to_lisp e =
  case e of
    Char c -> S.Char c
    String s -> S.String s
    Integer i -> S.Number i
    Double d -> S.Float d
    Symbol s -> S.Atom s
    Array l -> if null l then S.List [S.Atom "quote", S.nullLisp] else S.List (S.Atom "list" : map exp_to_lisp l)
    Set lhs rhs -> S.List [S.Atom "set!", exp_to_lisp lhs, exp_to_lisp rhs]
    App f a -> S.List (exp_to_lisp f : map exp_to_lisp a)
    Seq p q -> S.List (S.Atom "begin" : map exp_to_lisp [p, q])
    Lambda p x -> S.List [S.Atom "lambda", S.List (map S.Atom p), exp_to_lisp x]
    Let d x -> let_to_lisp d x
    Define lhs rhs -> S.List [S.Atom "define", S.Atom lhs, exp_to_lisp rhs]
    Nil -> S.List [S.Atom "quote", S.nullLisp]

-- * Translate

type Parser = String -> Stc.StcInitializerDefinition

-- | If dfn is True translate to Exp sequence, else to Let.
toExp :: Parser -> Bool -> String -> [Exp]
toExp f dfn =
  if dfn
    then stcInitializerDefinition_to_exp_seq . f
    else return . stcInitializerDefinition_to_let_exp . f

-- | Lex, parse and convert .stc expression to Exp.
stcToExp :: Bool -> String -> [Exp]
stcToExp = toExp Stc.stcParseToStc

splToExp :: Bool -> String -> [Exp]
splToExp = toExp Stc.splParseToStc

{- | Viewer for translator. Reads Stc expression, prints re-written Lisp expression.

>>> let rw = init . toLispViewer False Stc.stcParseToStc
>>> rw "$c"
"#\\c"

>>> rw "\"str\""
"\"str\""

>>> rw "'sym'"
"(quote sym)"

>>> rw "123"
"123"

>>> rw "1.2"
"1.2"

>>> rw "['sym', 123, 1.2]"
"(list (quote sym) 123 1.2)"

>>> rw "x = 1"
"(set! x 1)"

>>> rw "f(x)"
"(f x)"

>>> rw "x.f"
"(f x)"

>>> rw "x.f(y)"
"(f x y)"

>>> rw "f()"
"(f)"

>>> rw "f(x).g(y)"
"(g (f x) y)"

>>> rw "{}"
"(lambda () (quote ()))"

>>> rw "{arg x; x * 2}"
"(lambda (x) (* x 2))"

>>> rw "{arg x; var y = x * 2; y + 3}"
"(lambda (x) (let ((y (* x 2))) (+ y 3)))"

>>> rw "var x = 1; var y = 2; x + y"
"(let* ((x 1) (y 2)) (+ x y))"

>>> let rw = init . toLispViewer True Stc.stcParseToStc
>>> rw "var x = 1; var y = 2; x + y"
"(define x 1)\n(define y 2)\n(+ x y)"
-}
toLispViewer :: Bool -> Parser -> String -> String
toLispViewer dfn = toRenamedLispViewer dfn []

{- | Viewer for translator with renamer. Reads Stc expression, prints re-written Lisp expression.

>>> let rw = init . toRenamedLispViewer False [("+","add")] Stc.stcParseToStc
>>> rw "1 + 2"
"(add 1 2)"
-}
toRenamedLispViewer :: Bool -> [(String, String)] -> Parser -> String -> String
toRenamedLispViewer dfn tbl f =
  unlines
    . map (L.sexp_show . exp_to_lisp . exp_rename tbl)
    . toExp f dfn
