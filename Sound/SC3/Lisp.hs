{-

NOTE: the primitive lambda form is monadic, ie. λx → y

For parsers that allow unicode (ie. Ethier) the primitive lambda can be written λ

-}
module Sound.SC3.Lisp where

import Control.Concurrent {- base -}
import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}
import Data.Maybe {- base -}
import System.Directory {- directory -}
import System.Environment {- base -}
import System.Exit {- base -}
import System.FilePath {- filepath -}
import System.IO {- base -}

import qualified Data.Map as Map {- containers -}

import Sound.SC3.Lisp.Env {- hsc3-lisp -}
import Sound.SC3.Lisp.Type {- hsc3-lisp -}
import qualified Sound.SC3.Lisp.Parse.Ethier as Parse {- hsc3-lisp -}

-- * CELL

atom :: Cell a -> Maybe a
atom c =
    case c of
      Atom a -> Just a
      _ -> Nothing

type CellVM t r = EnvMonad IO (Cell t) r
type LispVM t = CellVM t (Cell t)

maybe_to_err :: String -> Maybe r -> EnvMonad IO t r
maybe_to_err msg = maybe (throwError msg) return

atom_err :: Lisp_Ty r => Cell r -> EnvMonad IO t r
atom_err c = maybe_to_err ("not atom: " ++ show c) (atom c)

from_list :: [Cell a] -> Cell a
from_list = foldr Cons Nil

l_false :: Lisp_Ty a => Cell a
l_false = Atom (ty_from_bool False)

l_true :: Lisp_Ty a => Cell a
l_true = Atom (ty_from_bool True)

l_equal :: Lisp_Ty a => Cell a -> Cell a
l_equal lhs = Fun (\rhs -> if lhs == rhs then l_true else l_false)

-- * EVAL / APPLY

trace :: Show t => Trace_Level -> String -> t -> EnvMonad IO a ()
trace (Trace_Level lvl) msg val = when (lvl < 3) (liftIO (putStrLn ("trace: " ++ msg ++ ": " ++ show val)))

{- | Apply works by:
   1. saving the current environment (c_env);
   2. extending the lambda environment (l_env) with the binding (nm,arg) and making this the current environment;
   3. running 'eval' of /code/ in the current (extended l_env) environment and saving the result;
   4. restoring the saved environment (c_env);
   5. returning the saved result
-}
apply_lambda :: Lisp_Ty t => Env (Cell t) -> String -> Cell t -> Cell t -> LispVM t
apply_lambda l_env nm code arg = do
  c_env <- get
  r <- env_lookup_m nm l_env
  when (isJust r) (trace (Trace_Level 3) "env_add_frame: shadowing" nm)
  put =<< liftIO (env_add_frame [(nm,arg)] l_env)
  res <- eval code
  put c_env
  return res

-- | Functions are one argument, but allow (+ 1 2) for ((+ 1) 2).
apply :: Lisp_Ty a => Cell a -> Cell a -> Cell a -> LispVM a
apply lhs arg var_arg = do
  let msg = from_list [Symbol "lhs:",lhs,Symbol "rhs:",arg,Symbol "rem:",var_arg]
  r <- case lhs of
         Fun f -> eval arg >>= return . f
         Proc f -> eval arg >>= f
         Lambda env nm code -> eval arg >>= apply_lambda env nm code
         _ -> throwError ("apply: invalid lhs: " ++ show msg)
  case var_arg of
    Nil -> return r
    Cons e l' -> apply r e l'
    _ -> throwError ("apply: invalid var-arg: " ++ show msg)

l_apply :: Lisp_Ty a => Cell a -> LispVM a
l_apply c = do
  let Cons lhs rhs = c
  (p,l) <- case rhs of
             Nil -> return (Nil,Nil)
             Cons p' q' -> return (p',q')
             _ -> throwError ("apply: rhs not nil or cons: " ++ show rhs)
  f <- eval lhs
  case f of
    Macro _ -> throwError ("apply: macro? " ++ show c)
    _ -> apply f p l

l_quote :: Cell a -> Cell a
l_quote c = Cons (Symbol "quote") (Cons c Nil)

l_lambda :: String -> Cell a -> LispVM a
l_lambda nm code = get >>= \env -> return (Lambda env nm code)

l_set :: Lisp_Ty a => String -> Cell a -> LispVM a
l_set nm def = get >>= \env -> eval def >>= \def' -> liftIO (env_set env nm def') >> return Nil

l_if :: Lisp_Ty a => Cell a -> Cell a -> Cell a -> LispVM a
l_if p t f = eval p >>= \p' -> if p' == l_false then eval f else eval t

eval :: Lisp_Ty a => Cell a -> LispVM a
eval c =
    case c of
      String _ -> return c
      Atom _ -> return c
      Nil -> return c
      Symbol nm -> get >>= \env -> env_lookup nm env
      Cons (Symbol "set!") (Cons (Symbol nm) (Cons def Nil)) -> l_set nm def
      Cons (Symbol "if") (Cons p (Cons t (Cons f Nil))) -> l_if p t f
      Cons (Symbol "quote") (Cons code Nil) -> return code
      Cons (Symbol "λ") (Cons (Symbol nm) (Cons code Nil)) -> l_lambda nm code -- λ PRIMITIVE
      Cons (Symbol "macro") (Cons code Nil) -> fmap Macro (eval code)
      Cons (Symbol "fork") (Cons code Nil) -> do
             e <- get
             let f = env_copy e >>= runStateT (runExceptT (eval code)) >> return ()
             _ <- liftIO (forkIO f)
             return Nil
      Cons _ _ -> l_apply c
      _ -> throwError ("eval: illegal form: " ++ show c)

l_mapM :: Lisp_Ty a => (Cell a -> LispVM a) -> Cell a -> LispVM a
l_mapM f c =
    case c of
      Nil -> return Nil
      Cons lhs rhs -> f lhs >>= \lhs' -> fmap (Cons lhs') (l_mapM f rhs)
      _ -> throwError ("l_mapM: not list? " ++ show c)

-- | If /c/ is a Macro call expand it, and then expand the result.
-- Do not expand quoted forms.
expand :: Lisp_Ty a => Cell a -> LispVM a
expand c = do
  case c of
    Cons lhs rhs ->
        case lhs of
          Symbol "quote" -> return c
          Symbol sym ->
              do env <- get
                 lhs' <- env_lookup_m sym env
                 case lhs' of
                   Just (Macro f) ->
                       do rhs' <- l_mapM expand rhs
                          c' <- apply f (l_quote rhs') Nil
                          expand c'
                   _ -> l_mapM expand c
          _ -> l_mapM expand c
    _ -> return c

-- * LOAD

eval_str :: Lisp_Ty t => Trace_Level -> String -> CellVM t [Cell t]
eval_str lvl str = do
  trace lvl "eval_str" str
  l <- Parse.parse_sexp_vm str
  trace lvl "eval_str" l
  mapM (\e -> Parse.sexp_to_cell e >>= expand >>= eval) l

load :: Lisp_Ty t => Cell t -> CellVM t ()
load c = do
  case c of
    String nm -> do
               x <- liftIO (doesFileExist nm)
               when (not x) (throwError ("load: file missing: " ++ nm))
               liftIO (putStrLn nm >> readFile nm) >>= eval_str (Trace_Level 5) >> return ()
    _ -> throwError ("load: " ++ show c)

load_files :: Lisp_Ty t => [String] -> CellVM t ()
load_files nm = do
  r <- liftIO (lookupEnv "HSC3_LISP_DIR")
  case r of
    Nothing -> throwError "HSC3_LISP_DIR not set"
    Just dir -> mapM_ load (map (String . (dir </>)) nm)

-- * CORE

l_write_char :: Lisp_Ty a => Cell a -> LispVM a
l_write_char c = atom_err c >>= \a -> liftIO (putChar (toEnum (ty_to_int a)) >> return Nil)

l_string_to_symbol :: Lisp_Ty a => Cell a -> Cell a
l_string_to_symbol c =
    case c of
      String s -> Symbol s
      _ -> Error ("string->symbol: " ++ show c)

l_string_append :: Lisp_Ty a => Cell a -> Cell a -> Cell a
l_string_append p q =
    case (p,q) of
      (String r,String s) -> String (r ++ s)
      _ -> Error ("string-append: " ++ show (p,q))

l_write_string :: Lisp_Ty a => Cell a -> LispVM a
l_write_string c =
    case c of
      String s -> liftIO (putStr s) >> return Nil
      _ -> throwError ("write-string: " ++ show c)

l_env_print :: Lisp_Ty a => Cell a -> LispVM a
l_env_print x = do
  e <- get
  liftIO (env_print e)
  return x

core_dict :: Lisp_Ty a => Dict (Cell a)
core_dict =
    Map.fromList
    [("#t",l_true)
    ,("#f",l_false)
    ,("car",Fun (\c -> case c of {Cons lhs _ -> lhs; _ -> Error ("car: " ++ show c)}))
    ,("cdr",Fun (\c -> case c of {Cons _ rhs -> rhs; _ -> Error ("cdr: " ++ show c)}))
    ,("cons",Fun (\lhs -> Fun (\rhs -> Cons lhs rhs)))
    ,("env-print", Proc l_env_print)
    ,("equal?",Fun l_equal)
    ,("error",Proc (\c -> throwError ("error: " ++ show c)))
    ,("eval",Proc (\c -> eval c >>= eval))
    ,("exit",Proc (\_ -> liftIO exitSuccess))
    ,("expand",Proc (\c -> expand c))
    ,("list?",Fun (Atom . ty_from_bool . is_list))
    ,("load",Proc (\c -> load c >> return Nil))
    ,("null?",Fun (\c -> case c of {Nil -> l_true; _ -> l_false}))
    ,("pair?",Fun (\c -> case c of {Cons _ _ -> l_true; _ -> l_false}))
    ,("show",Fun (String . show))
    ,("string->symbol",Fun l_string_to_symbol)
    ,("string-append",Fun (\p -> Fun (\q -> l_string_append p q)))
    ,("string?",Fun (\c -> case c of {String _ -> l_true; _ -> l_false}))
    ,("symbol?",Fun (\c -> case c of {Symbol _ -> l_true; _ -> l_false}))
    ,("write-char",Proc l_write_char)
    ,("write-string",Proc l_write_string)
    ]

-- * REPL

get_sexp :: String -> Handle -> IO String
get_sexp s h = do
  l <- hGetLine h -- no eol
  r <- hReady h
  let s' = s ++ (l ++ "\n")
  if r then get_sexp s' h else return s'

repl_cont :: Lisp_Ty a => Env (Cell a) -> IO ()
repl_cont env = do
  str <- get_sexp "" stdin
  (r,env') <- runStateT (runExceptT (eval_str (Trace_Level 3) str)) env
  case r of
    Left msg -> putStrLn ("error: " ++ msg) >> repl_cont env
    Right res -> mapM_ (\res' -> putStrLn ("result: " ++ show res')) res >> repl_cont env'

repl_init :: Lisp_Ty t => Env (Cell t) -> CellVM t () -> IO ()
repl_init env initialise = do
  (r,env') <- runStateT (runExceptT initialise) env
  case r of
    Left msg -> error ("repl_init: init error: " ++ msg)
    Right () -> repl_cont env'
