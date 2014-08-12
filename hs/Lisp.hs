module Lisp where

import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}
import Data.Char {- base -}
import Data.IORef {- base -}
import qualified Data.Map as M {- containers -}
import System.Directory {- directory -}
import System.Environment {- base -}
import System.Exit {- base -}
import System.FilePath {- filepath -}
import System.IO {- base -}

import qualified Language.Scheme.Parser as S {- husk-scheme -}
import qualified Language.Scheme.Types as S {- husk-scheme -}

-- * Types

class (Eq a,Ord a,Num a,Fractional a) => Lisp_Ty a where
    ty_show :: a -> String -- ^ String representation of /a/, pretty printer.
    ty_to_int :: a -> Int -- ^ Coercion, ie. for Char.
    ty_from_bool :: Bool -> a -- ^ Boolean value represented in /a/, by convention @1@ and @0@.

type Dict a = M.Map String (Cell a)

data Env a = Frame (IORef (String,Cell a)) (Env a) | Toplevel (IORef (Dict a))

data Cell a = Symbol String | String String
            | Atom a
            | Nil | Cons (Cell a) (Cell a)
            | Fun (Cell a -> Cell a)
            | Proc (Cell a -> VM a (Cell a))
            | Lambda (Env a) String (Cell a)
            | Macro (Cell a)
            | Error String

instance Eq a => Eq (Cell a) where
    lhs == rhs =
        case (lhs,rhs) of
          (Atom p,Atom q) -> p == q
          (String p,String q) -> p == q
          (Symbol p,Symbol q) -> p == q
          (Nil,Nil) -> True
          (Cons p p',Cons q q') -> p == q && p' == q'
          _ -> False -- error "EQ"

type VM a r = ExceptT String (StateT (Env a) IO) r

-- * ENV

env_print :: Lisp_Ty t => Env t -> IO ()
env_print e =
    case e of
      Frame f e' -> readIORef f >>= print >> env_print e'
      Toplevel d -> readIORef d >>= print

env_empty :: IO (Env a)
env_empty = do
  d <- newIORef M.empty
  return (Toplevel d)

env_lookup :: String -> Env a -> VM a (Cell a)
env_lookup w e =
    case e of
      Frame f e' -> do
             (k,v) <- liftIO (readIORef f)
             if w == k then return v else env_lookup w e'
      Toplevel d -> do
             d' <- liftIO (readIORef d)
             case M.lookup w d' of
               Just r -> return r
               Nothing -> throwError ("ENV-LOOKUP: " ++ w)

env_add_frame :: String -> (Cell a) -> Env a -> IO (Env a)
env_add_frame k v e = do
  f <- newIORef (k,v)
  return (Frame f e)

env_set :: Env a -> String -> Cell a -> IO ()
env_set e nm c =
    case e of
      Frame f e' -> do
             (k,_) <- liftIO (readIORef f)
             if nm == k then writeIORef f (nm,c) else env_set e' nm c
      Toplevel d -> modifyIORef d (M.insert nm c)

gen_toplevel :: Lisp_Ty a => Dict a -> IO (Env a)
gen_toplevel = fmap Toplevel . newIORef

-- * CELL

atom :: Cell a -> Maybe a
atom c =
    case c of
      Atom a -> Just a
      _ -> Nothing

maybe_to_err :: String -> Maybe a -> VM a a
maybe_to_err msg = maybe (throwError msg) return

atom_err :: Lisp_Ty a => Cell a -> VM a a
atom_err c = maybe_to_err ("NOT ATOM: " ++ show c) (atom c)

is_list :: Eq a => Cell a -> Bool
is_list c =
    case c of
      Cons _ c' -> c' == Nil || is_list c'
      _ -> False

to_list :: Lisp_Ty a => Cell a -> [Cell a]
to_list l =
    case l of
      Nil -> []
      Cons e l' -> e : to_list l'
      _ -> [Error "NOT LIST?"]

from_list :: [Cell a] -> Cell a
from_list = foldr Cons Nil

list_pp :: Lisp_Ty a => Cell a -> String
list_pp c = "(" ++ unwords (map show (to_list c)) ++ ")"

instance Lisp_Ty a => Show (Cell a) where
    show c =
        case c of
          Atom a -> ty_show a
          Symbol s -> s
          String s -> show s
          Nil -> "NIL"
          Cons p q -> if is_list c then list_pp c else concat ["(CONS ",show p," ",show q,")"]
          Fun _ -> "FUN"
          Proc _ -> "PROC"
          Lambda _ nm code -> concat ["(λ ",nm," ",show code,")"]
          Macro m -> "MACRO: " ++ show m
          Error msg -> "ERROR: " ++ msg

l_false :: Lisp_Ty a => Cell a
l_false = Atom (ty_from_bool False)

l_true :: Lisp_Ty a => Cell a
l_true = Atom (ty_from_bool True)

l_equal :: Lisp_Ty a => Cell a -> Cell a
l_equal lhs = Fun (\rhs -> if lhs == rhs then l_true else l_false)

-- * SEXP

type SEXP = S.LispVal

parse_sexps :: String -> VM a [SEXP]
parse_sexps = either (throwError . show) return . S.readExprList

parse_sexp :: String -> VM a SEXP
parse_sexp = either (throwError . show) return . S.readExpr

parse_cell' :: Lisp_Ty a => SEXP -> VM a (Cell a)
parse_cell' sexp =
    case sexp of
      S.Number n -> return (Atom (fromIntegral n))
      S.Float n -> return (Atom (realToFrac n))
      S.Rational n -> return (Atom (fromRational n))
      S.Atom nm -> return (Symbol nm)
      S.String s -> return (String s)
      S.Bool b -> return (Atom (ty_from_bool b))
      S.List [] -> return Nil
      S.List (e : l) -> do
                e' <- parse_cell' e
                l' <- parse_cell' (S.List l)
                return (Cons e' l')
      _ -> throwError (show "PARSE-CELL")

parse_cell :: Lisp_Ty a => String -> VM a (Cell a)
parse_cell str = parse_sexp str >>= parse_cell'

-- * EVAL / APPLY

apply_lambda :: Lisp_Ty a => Env a -> String -> Cell a -> Cell a -> VM a (Cell a)
apply_lambda l_env nm code arg = do
  -- save caller environment
  c_env <- get
  -- store extended lambda environment
  put =<< liftIO (env_add_frame nm arg l_env)
  -- eval code in lambda environment
  res <- eval code
  -- restore caller environment
  put c_env
  -- return result
  return res

-- | Functions are one argument, but allow (+ 1 2) for ((+ 1) 2).
apply :: Lisp_Ty a => Cell a -> Cell a -> Cell a -> VM a (Cell a)
apply lhs arg var_arg = do
  let msg = from_list [Symbol "LHS:",lhs,Symbol "RHS:",arg,Symbol "REM:",var_arg]
  r <- case lhs of
         Fun f -> eval arg >>= return . f
         Proc f -> eval arg >>= f
         Lambda env nm code -> eval arg >>= apply_lambda env nm code
         _ -> throwError ("APPLY: INVALID LHS: " ++ show msg)
  case var_arg of
    Nil -> return r
    Cons e l' -> apply r e l'
    _ -> throwError ("APPLY: INVALID VAR-ARG: " ++ show msg)

l_apply :: Lisp_Ty a => Cell a -> VM a (Cell a)
l_apply c = do
  let Cons lhs rhs = c
  (p,l) <- case rhs of
             Nil -> return (Nil,Nil)
             Cons p' q' -> return (p',q')
             _ -> throwError ("APPLY: RHS not NIL or CONS: " ++ show rhs)
  f <- eval lhs
  case f of
    Macro f' -> apply f' (l_quote rhs) Nil >>= eval
    _ -> apply f p l

l_quote :: Cell a -> Cell a
l_quote c = Cons (Symbol "quote") (Cons c Nil)

l_lambda :: String -> Cell a -> VM a (Cell a)
l_lambda nm code = get >>= \env -> return (Lambda env nm code)

l_set :: Lisp_Ty a => String -> Cell a -> VM a (Cell a)
l_set nm def = get >>= \env -> eval def >>= \def' -> liftIO (env_set env nm def') >> return Nil

l_if :: Lisp_Ty a => Cell a -> Cell a -> Cell a -> VM a (Cell a)
l_if p t f = eval p >>= \p' -> if p' == l_false then eval f else eval t

eval :: Lisp_Ty a => Cell a -> VM a (Cell a)
eval c =
    case c of
      String _ -> return c
      Atom _ -> return c
      Nil -> return c
      Symbol nm -> get >>= \env -> env_lookup nm env
      Cons (Symbol "set!") (Cons (Symbol nm) (Cons def Nil)) -> l_set nm def
      Cons (Symbol "if") (Cons p (Cons t (Cons f Nil))) -> l_if p t f
      Cons (Symbol "quote") (Cons code Nil) -> return code
      Cons (Symbol "λ") (Cons (Symbol nm) (Cons code Nil)) -> l_lambda nm code
      Cons (Symbol "macro") (Cons code Nil) -> fmap Macro (eval code)
      Cons _ _ -> l_apply c
      _ -> throwError ("EVAL: ILLEGAL FORM: " ++ show c)

-- * LOAD

load :: Lisp_Ty a => Cell a -> VM a ()
load c = do
  case c of
    String nm -> do
               x <- liftIO (doesFileExist nm)
               when (not x) (throwError ("LOAD: FILE MISSING: " ++ nm))
               str <- liftIO (readFile nm)
               sexps <- parse_sexps str
               cells <- mapM parse_cell' sexps
               mapM_ eval cells
    _ -> throwError ("LOAD: " ++ show c)

load_files :: Lisp_Ty a => [String] -> VM a ()
load_files nm = do
  r <- liftIO (lookupEnv "HSC3_LISP_DIR")
  case r of
    Nothing -> throwError "HSC3_LISP_DIR NOT SET"
    Just dir -> mapM_ load (map (String . (dir </>)) nm)

-- * CORE

l_write_char :: Lisp_Ty a => Cell a -> VM a (Cell a1)
l_write_char c = atom_err c >>= \a -> liftIO (putChar (toEnum (ty_to_int a)) >> return Nil)

core_dict :: Lisp_Ty a => Dict a
core_dict =
    M.fromList
    [("#t",l_true)
    ,("#f",l_false)
    ,("symbol?",Fun (\c -> case c of {Symbol _ -> l_true; _ -> l_false}))
    ,("string?",Fun (\c -> case c of {String _ -> l_true; _ -> l_false}))
    ,("cons",Fun (\lhs -> Fun (\rhs -> Cons lhs rhs)))
    ,("car",Fun (\c -> case c of {Cons lhs _ -> lhs; _ -> Error ("CAR: " ++ show c)}))
    ,("cdr",Fun (\c -> case c of {Cons _ rhs -> rhs; _ -> Error ("CDR: " ++ show c)}))
    ,("null?",Fun (\c -> case c of {Nil -> l_true; _ -> l_false}))
    ,("pair?",Fun (\c -> case c of {Cons _ _ -> l_true; _ -> l_false}))
    ,("list?",Fun (Atom . ty_from_bool . is_list))
    ,("equal?",Fun l_equal)
    ,("write-char",Proc l_write_char)
    ,("display",Proc (\c -> liftIO (putStr (show c)) >> return c))
    ,("load",Proc (\c -> load c >> return Nil))
    ,("eval",Proc (\c -> eval c >>= eval))
    ,("error",Proc (\c -> throwError ("ERROR: " ++ show c)))
    ,("exit",Proc (\_ -> liftIO exitSuccess))]

-- * REPL

get_sexp :: String -> Handle -> IO String
get_sexp s h = do
  l <- hGetLine h
  r <- hReady h
  let s' = s ++ l
  if r then get_sexp s' h else return (map toLower s')

repl' :: Lisp_Ty a => Env a -> IO ()
repl' env = do
  str <- get_sexp "" stdin
  (r,env') <- runStateT (runExceptT (parse_cell str >>= eval)) env
  case r of
    Left msg -> putStrLn ("ERROR: " ++ msg) >> repl' env
    Right res -> putStrLn ("RESULT: " ++ show res) >> repl' env'

repl :: Lisp_Ty a => Env a -> VM a () -> IO ()
repl env initialise = do
  (r,env') <- runStateT (runExceptT initialise) env
  case r of
    Left msg -> error ("REPL: INIT ERROR: " ++ msg)
    Right () -> repl' env'
