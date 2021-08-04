import Control.Monad {- base -}
import Data.Char {- base -}
import Data.Maybe {- base -}
import System.Environment {- base -}

import qualified Safe {- safe -}

import qualified Control.Monad.Except as Monad {- mtl -}

import qualified Data.Map as Map {- containers -}

import Sound.OSC {- hosc -}
import Sound.SC3 {- hsc3 -}
import Sound.SC3.UGen.Plain {- hsc3 -}

import qualified Sound.SC3.UGen.Protect as Protect {- hsc3-rw -}

import qualified Sound.SC3.UGen.Dot as Dot {- hsc3-dot -}

import Sound.SC3.Lisp {- hsc3-lisp -}
import qualified Sound.SC3.Lisp.Env as Env {- hsc3-lisp -}
import Sound.SC3.Lisp.Type {- hsc3-lisp -}

ugen_to_int :: String -> UGen -> Int
ugen_to_int c u =
    let err = error ("ugen_to_int: " ++ c ++ ": " ++ show u)
        f = floor . fromMaybe err . u_constant
    in f u

instance Lisp_Ty UGen where
    ty_show = ugen_concise_pp
    ty_to_int = ugen_to_int "ty_to_int"
    ty_from_bool t = if t then 1 else 0

lift_io :: IO () -> LispVM t
lift_io f = liftIO f >> return Nil

constant_err :: Expr UGen -> Double
constant_err c =
    case fmap constant_opt (atom c) of
      Just (Just n) -> n
      _ -> error "not constant?"

ugen_to_double :: String -> UGen -> Double
ugen_to_double c u =
    let err = error ("ugen_to_double: " ++ c ++ ": " ++ show u)
        f = fromMaybe err . u_constant
    in f u

l_mk_ctl :: Expr UGen -> LispVM UGen
l_mk_ctl c = do
  let l = to_list c
  [rt,nm,df] <- if length l == 3 then return l else Monad.throwError ("mk-ctl: incorrect input: " ++ show c)
  rt' <- case rt of
           Symbol sym -> return (fromJust (rate_parse (map toUpper sym)))
           _ -> Monad.throwError ("mk-ctl: rate: " ++ show rt)
  nm' <- case nm of
           String str -> return str
           _ -> Monad.throwError ("mk-ctl: name not string: " ++ show nm)
  df' <- case df of
          Atom u -> return (ugen_to_double "ctl-def" u)
          _ -> Monad.throwError ("mk-ctl: def: " ++ show df)
  return (Atom (control rt' nm' df'))

l_make_mce :: Expr UGen -> Env.EnvMonad IO String t UGen
l_make_mce c = fmap mce (mapM (atom_note "l_make_mce") (to_list c))

l_as_ugen_input :: Expr UGen -> Env.EnvMonad IO String t UGen
l_as_ugen_input c = if is_list c then l_make_mce c else atom_note "l_as_ugen_input" c

l_mk_ugen :: Expr UGen -> LispVM UGen
l_mk_ugen c = do
  let l = to_list c
  [nm,rt,inp,inp_mce,outp,sp] <- if length l == 6
                                 then return l
                                 else Monad.throwError ("mk-ugen: incorrect input: " ++ show c)
  inp_mce' <- case inp_mce of
                Atom u -> return (mceChannels u)
                Nil -> return []
                _ -> Monad.throwError ("mk-ugen: mce-input: " ++ show inp_mce)
  sp' <- case sp of
           Atom u -> return (Special (ugen_to_int "special" u))
           Nil -> return (Special 0)
           _ -> Monad.throwError "mk-ugen: special?"
  uid <- fmap UId (liftIO generateUId)
  inp' <- fmap (++ inp_mce') (mapM l_as_ugen_input (to_list inp))
  rt' <- case rt of
           Symbol sym -> return (fromJust (rate_parse (map toUpper sym)))
           Cons _ _ -> do
             let f = rateOf . Safe.atNote ("mk-ugen: rate: " ++ show c) inp' . ugen_to_int "rate"
             fmap maximum (mapM (fmap f . atom_note "mk-ugen: rate") (to_list rt))
           _ -> Monad.throwError ("mk-ugen: rate: " ++ show rt)
  nm' <- case nm of
           String str -> return str
           _ -> Monad.throwError ("mk-ugen: name not string: " ++ show nm)
  let outp' = floor (constant_err outp)
  return (Atom (ugen_optimise_const_operator (mk_plain rt' nm' inp' outp' sp' uid)))

l_is_number :: Expr UGen -> Expr UGen
l_is_number c =
    case c of
      Atom u -> if isConstant u then l_true else l_false
      _ -> l_false

l_is_mce :: Expr UGen -> Expr UGen
l_is_mce c =
    case c of
      Atom u -> if isMce u then l_true else l_false
      _ -> l_false

l_is_procedure :: Expr UGen -> Expr UGen
l_is_procedure c =
    case c of
      Fun _ -> l_true
      Proc _ -> l_true
      Lambda _ _ _ -> l_true
      Macro _ -> l_true
      _ -> l_false

l_clone_star :: Expr UGen -> LispVM UGen
l_clone_star c =
    case to_list c of
      [Atom k,Atom n,Atom u] ->
         let k' = ugen_to_int "clone-k" k
             n' = ugen_to_int "clone-n" n
         in return (Atom (Protect.uclone (const False) k' n' u))
      _ -> Monad.throwError ("clone*: " ++ show c)

l_play_at_star :: Expr UGen -> LispVM UGen
l_play_at_star c =
    case to_list c of
     [_,Atom u,Atom nid,Atom act,Atom grp] ->
         let nid' = ugen_to_int "play-at: nid" nid
             act' = ugen_to_int "play-at: act" act
             grp' = ugen_to_int "play-at: grp" grp
         in lift_io (withSC3 (playAt (nid',toEnum act',grp',[]) u)) >>
            return Nil
     _ -> Monad.throwError ("play-at*: " ++ show c)

l_thread_sleep :: Expr UGen -> LispVM UGen
l_thread_sleep c = do
    u <- atom_note "l_thread_sleep" c
    liftIO (pauseThread (ugen_to_double "pause" u))
    return Nil

expr_to_datum :: Expr UGen -> ExprVM t Datum
expr_to_datum c =
    case c of
      Symbol str -> return (string str)
      String str -> return (string str)
      Atom (UGen (CConstant (Constant n))) -> return (float n)
      _ -> Monad.throwError ("expr-to-datum: " ++ show c)

expr_to_message :: Expr UGen -> ExprVM t Message
expr_to_message c =
    case to_list_m c of
      Just (String addr : l) -> mapM expr_to_datum l >>= \l' -> return (Message addr l')
      _ -> Monad.throwError ("expr-to-message: " ++ show c)

l_async_star :: Expr UGen -> LispVM t
l_async_star c = expr_to_message c >>= \c' -> lift_io (withSC3 (void (async c')))

l_send_star :: Expr UGen -> LispVM t
l_send_star c = expr_to_message c >>= \c' -> lift_io (withSC3 (void (sendMessage c')))

ugen_dict :: Env.Dict String (Expr UGen)
ugen_dict =
    Map.fromList
    [("number?",Fun l_is_number)
    ,("mce?",Fun l_is_mce)
    ,("string?",Fun (\c -> case c of {String _ -> l_true; _ -> l_false}))
    ,("symbol?",Fun (\c -> case c of {Symbol _ -> l_true; _ -> l_false}))
    ,("procedure?",Fun l_is_procedure)
    ,("mk-ctl",Proc l_mk_ctl)
    ,("mk-ugen",Proc l_mk_ugen)
    ,("clone*",Proc l_clone_star)
    ,("make-mce",Proc (fmap Atom . l_make_mce))
    ,("mceChannels",Proc (\c -> fmap (from_list . map Atom . mceChannels) (atom_note "mceChannels" c)))
    ,("mceDegree",Proc (\c -> fmap (Atom . constant . mceDegree_err) (atom_note "mceDegree" c)))
    ,("Mrg",Proc (\c -> fmap (Atom . mrg) (mapM (atom_note "Mrg") (to_list c))))
    ,("show-graph",Proc (\c -> atom_note "show-graph" c >>= \u -> lift_io (Dot.draw u)))
    ,("play-at*",Proc l_play_at_star)
    ,("reset*",Proc (\_ -> lift_io (withSC3 reset)))
    ,("threadSleep",Proc l_thread_sleep)
    ,("utcr",Proc (\_ -> liftIO time >>= return . Atom . constant))
    ,("displayServerStatus",Proc (\_ -> lift_io (withSC3 serverStatus >>= mapM_ putStrLn)))
    ,("async*",Proc l_async_star)
    ,("send*",Proc l_send_star)
    ,("unrand",Proc (\c -> atom_note "urand" c >>= \u -> return (Atom (ugen_optimise_ir_rand u))))]

main :: IO ()
main = do
  putStrLn "hsc3-lisp"
  env <- Env.envNewFrom (Map.unions [core_dict,ugen_dict]) :: IO (Env.Env String (Expr UGen))
  let lib = ["stdlib.scm"
            ,"scheme.scm"
            ,"rhs.prereq.scm"
            ,"rhs.scm" -- sw/rhs
            ,"rsc3.prereq.scm"
            ,"ugen.scm" -- sw/rsc3
            ,"hsc3.scm"
            ,"rsc3.scm" -- sw/rsc3
            ,"alias.scm"
            ]
  a <- getArgs
  repl_init env (load_files (lib ++ a))