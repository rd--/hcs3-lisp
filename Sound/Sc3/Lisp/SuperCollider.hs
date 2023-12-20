-- | Rewrite a subset of SuperCollider (specifically .scs & .stc) as Lisp.
module Sound.Sc3.Lisp.SuperCollider where

import Data.Maybe {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Ast as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc {- stsc3 -}

import qualified Language.Scheme.Types as S {- husk-scheme -}

import qualified Sound.Sc3.Lisp.Parse.Ethier as L {- hsc3-lisp -}
import Sound.Sc3.Lisp.Scs {- hsc3-lisp -}

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

scStatements_to_exp :: Sc.ScStatements -> Exp
scStatements_to_exp x =
  case x of
    Sc.ScStatementsReturn (Sc.ScReturnStatement p) -> App (Symbol "return") [scExpression_to_exp p]
    Sc.ScStatementsExpression p q ->
      case q of
        Nothing -> scExpression_to_exp p
        Just q' -> Seq (scExpression_to_exp p) (scStatements_to_exp q')

-- | Translated as Lambda with interior Let.  Alternately, Lambda could have a temporaries field.
scBlockBody_to_exp :: Sc.ScBlockBody -> Exp
scBlockBody_to_exp (Sc.ScBlockBody arg tmp stm) =
  let binder (p, q) = (p, maybe Nil scBasicExpression_to_exp q)
      body = Let (maybe [] (concatMap (map binder)) tmp) (maybe Nil scStatements_to_exp stm)
  in Lambda (map fst (fromMaybe [] arg)) body

scExpression_to_exp :: Sc.ScExpression -> Exp
scExpression_to_exp e =
  case e of
    Sc.ScExprAssignment p q -> Set (Symbol p) (scExpression_to_exp q)
    Sc.ScExprBasic p -> scBasicExpression_to_exp p

-- | rcv.msg(arg) translates are (msg (rcv : arg)).
scDotMessage_to_exp :: Exp -> Sc.ScDotMessage -> Exp
scDotMessage_to_exp rcv (Sc.ScDotMessage msg arg) = App (Symbol msg) (rcv : map scBasicExpression_to_exp arg)

scDotMessages_to_exp :: Exp -> [Sc.ScDotMessage] -> Exp
scDotMessages_to_exp rcv m =
  case m of
    [] -> rcv
    p : q -> scDotMessages_to_exp (scDotMessage_to_exp rcv p) q

scBinaryArgument_to_exp :: Sc.ScBinaryArgument -> Exp
scBinaryArgument_to_exp (Sc.ScBinaryArgument p q) = scDotMessages_to_exp (scPrimary_to_exp p) (fromMaybe [] q)

scBinaryMessage_to_exp :: Exp -> Sc.ScBinaryMessage -> Exp
scBinaryMessage_to_exp rcv (Sc.ScBinaryMessage p q) = App (Symbol p) [rcv, scBinaryArgument_to_exp q]

scBinaryMessages_to_exp :: Exp -> [Sc.ScBinaryMessage] -> Exp
scBinaryMessages_to_exp rcv m =
  case m of
    [] -> rcv
    p : q -> scBinaryMessages_to_exp (scBinaryMessage_to_exp rcv p) q

scMessages_to_exp :: Exp -> Sc.ScMessages -> Exp
scMessages_to_exp rcv m =
  case m of
    Sc.ScMessagesDot p q -> scBinaryMessages_to_exp (scDotMessages_to_exp rcv p) (fromMaybe [] q)
    Sc.ScMessagesBinary p -> scBinaryMessages_to_exp rcv p

scBasicExpression_to_exp :: Sc.ScBasicExpression -> Exp
scBasicExpression_to_exp (Sc.ScBasicExpression p q) =
  case q of
    Nothing -> scPrimary_to_exp p
    Just m -> scMessages_to_exp (scPrimary_to_exp p) m

scPrimary_to_exp :: Sc.ScPrimary -> Exp
scPrimary_to_exp x =
  case x of
    Sc.ScPrimaryIdentifier p -> Symbol p
    Sc.ScPrimaryLiteral p -> stLiteral_to_exp p
    Sc.ScPrimaryBlock p -> scBlockBody_to_exp p
    Sc.ScPrimaryExpression p -> scExpression_to_exp p
    Sc.ScPrimaryArrayExpression p -> Array (map scBasicExpression_to_exp p)
    Sc.ScPrimaryDictionaryExpression _ -> error "scPrimary_to_exp: dictionary..."
    Sc.ScPrimaryImplicitMessageSend p q -> App (Symbol p) (map scBasicExpression_to_exp q)

-- | Translate as let expression.  Alternately could translate as Seq of Set.
scInitializerDefinition_to_let_exp :: Sc.ScInitializerDefinition -> Exp
scInitializerDefinition_to_let_exp (Sc.ScInitializerDefinition _cmt tmp stm) =
  let binder (p, q) = (p, maybe Nil scBasicExpression_to_exp q)
      tmpSeq = concatMap (map binder) (fromMaybe [] tmp)
      stmExp = maybe Nil scStatements_to_exp stm
  in Let tmpSeq stmExp

-- | Translate as definition sequence with perhaps a subsequent program.
scInitializerDefinition_to_exp_seq :: Sc.ScInitializerDefinition -> [Exp]
scInitializerDefinition_to_exp_seq (Sc.ScInitializerDefinition _cmt tmp stm) =
  let binder (p, q) = Define p (maybe Nil scBasicExpression_to_exp q)
      tmpSeq = concatMap (map binder) (fromMaybe [] tmp)
      stmExp = maybe [] (return . scStatements_to_exp) stm
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

{- | Lex, parse and convert .stc expression to Exp.
     If dfn is True translate to Exp sequence, else to Let.
-}
scToExp :: Bool -> String -> [Exp]
scToExp dfn =
  let f = if dfn then scInitializerDefinition_to_exp_seq else return . scInitializerDefinition_to_let_exp
  in f . Sc.superColliderParserInitializerDefinition . Sc.alexScanTokens

{- | Viewer for translator. Reads Sc expression, prints re-written Lisp expression.

> rw = init . scToLispViewer False
> rw "$c"
> rw "\"str\""
> rw "'sym'" == "(quote sym)"
> rw "123" == "123"
> rw "1.2" == "1.2"
> rw "['sym', 123, 1.2]" == "(list (quote sym) 123 1.2)"
> rw "x = 1" == "(set! x 1)"
> rw "f(x)" == "(f x)"
> rw "x.f" == "(f x)"
> rw "x.f(y)" == "(f x y)"
> rw "f()" == "(f)"
> rw "f(x).g(y)" == "(g (f x) y)"
> rw "{}" == "(lambda () (quote ()))"
> rw "{arg x; x * 2}" == "(lambda (x) (* x 2))"
> rw "{arg x; var y = x * 2; y + 3}" == "(lambda (x) (let ((y (* x 2))) (+ y 3)))"
> rw "var x = 1; var y = 2; x + y" == "(let* ((x 1) (y 2)) (+ x y))"

> rw = init . scToLispViewer True
> rw "var x = 1; var y = 2; x + y" == "(define x 1)\n(define y 2)\n(+ x y)"
-}
scToLispViewer :: Bool -> String -> String
scToLispViewer dfn = scToRenamedLispViewer dfn []

{- | Viewer for translator with renamer. Reads Sc expression, prints re-written Lisp expression.

> rw = init . scToRenamedLispViewer False [("+","add")]
> rw "1 + 2" == "(add 1 2)"
-}
scToRenamedLispViewer :: Bool -> [(String, String)] -> String -> String
scToRenamedLispViewer dfn tbl =
  unlines
    . map (L.sexp_show . exp_to_lisp . exp_rename tbl)
    . scToExp dfn
