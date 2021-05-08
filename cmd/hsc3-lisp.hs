import Control.Monad {- base -}
import Data.Char {- base -}
import Data.Maybe {- base -}
import System.Environment {- base -}

import qualified Safe {- safe -}

import qualified Control.Monad.Except as Monad {- mtl -}

import qualified Data.Map as Map {- containers -}

import Sound.OSC {- hsc3 -}
import Sound.SC3 {- hsc3 -}
import Sound.SC3.UGen.Plain {- hsc3 -}
import Sound.SC3.UGen.PP {- hsc3 -}

import qualified Sound.SC3.UGen.Protect as Protect {- hsc3-rw -}

import qualified Sound.SC3.UGen.Dot as Dot {- hsc3-dot -}

import Sound.SC3.Lisp {- hsc3-lisp -}
import Sound.SC3.Lisp.Type {- hsc3-lisp -}

ugen_to_int :: String -> UGen -> Int
ugen_to_int c u =
    let err = error ("UGEN_TO_INT: " ++ c ++ ": " ++ show u)
        f = floor . fromMaybe err . u_constant
    in f u

instance Lisp_Ty UGen where
    ty_show = ugen_concise_pp
    ty_to_int = ugen_to_int "TY_TO_INT"
    ty_from_bool t = if t then 1 else 0

lift_io :: IO () -> VM a (Cell a)
lift_io f = liftIO f >> return Nil

constant_err :: Cell UGen -> Double
constant_err c =
    case fmap constant_opt (atom c) of
      Just (Just n) -> n
      _ -> error "NOT CONSTANT?"

ugen_to_double :: String -> UGen -> Double
ugen_to_double c u =
    let err = error ("UGEN_TO_DOUBLE: " ++ c ++ ": " ++ show u)
        f = fromMaybe err . u_constant
    in f u

l_mk_ctl :: Cell UGen -> VM UGen (Cell UGen)
l_mk_ctl c = do
  let l = to_list c
  [rt,nm,df] <- if length l == 3 then return l else Monad.throwError ("MK-CTL: INCORRECT INPUT: " ++ show c)
  rt' <- case rt of
           Symbol sym -> return (fromJust (rate_parse (map toUpper sym)))
           _ -> Monad.throwError ("MK-CTL: RATE: " ++ show rt)
  nm' <- case nm of
           String str -> return str
           _ -> Monad.throwError ("MK-CTL: NAME NOT STRING: " ++ show nm)
  df' <- case df of
          Atom u -> return (ugen_to_double "CTL-DEF" u)
          _ -> Monad.throwError ("MK-CTL: DEF: " ++ show df)
  return (Atom (control rt' nm' df'))

l_mk_ugen :: Cell UGen -> VM UGen (Cell UGen)
l_mk_ugen c = do
  let l = to_list c
  [nm,rt,inp,inp_mce,outp,sp,k] <- if length l == 7 then return l else Monad.throwError ("MK-UGEN: INCORRECT INPUT: " ++ show c)
  inp_mce' <- case inp_mce of
                Atom u -> return (mceChannels u)
                Nil -> return []
                _ -> Monad.throwError ("MK-UGEN: MCE-INPUT: " ++ show inp_mce)
  sp' <- case sp of
           Atom u -> return (Special (ugen_to_int "SPECIAL" u))
           Nil -> return (Special 0)
           _ -> Monad.throwError "MK-UGEN: SPECIAL?"
  k' <- case k of
          Atom u -> return (UId (ugen_to_int "UID" u))
          Nil -> return NoId
          _ -> Monad.throwError "MK-UGEN: UID?"
  inp' <- fmap (++ inp_mce') (mapM atom_err (to_list inp))
  rt' <- case rt of
           Symbol sym -> return (fromJust (rate_parse (map toUpper sym)))
           Cons _ _ -> do
             let f = rateOf . Safe.atNote ("MK-UGEN: RATE: " ++ show c) inp' . ugen_to_int "RATE"
             fmap maximum (mapM (fmap f . atom_err) (to_list rt))
           _ -> Monad.throwError ("MK-UGEN: RATE: " ++ show rt)
  nm' <- case nm of
           String str -> return str
           _ -> Monad.throwError ("MK-UGEN: NAME NOT STRING: " ++ show nm)
  let outp' = floor (constant_err outp)
  return (Atom (ugen_optimise_const_operator (mk_plain rt' nm' inp' outp' sp' k')))

l_is_number :: Cell UGen -> Cell UGen
l_is_number c =
    case c of
      Atom u -> if isConstant u then l_true else l_false
      _ -> l_false

l_is_procedure :: Cell UGen -> Cell UGen
l_is_procedure c =
    case c of
      Fun _ -> l_true
      Proc _ -> l_true
      Lambda _ _ _ -> l_true
      Macro _ -> l_true
      _ -> l_false

l_clone_star :: Cell UGen -> VM UGen (Cell UGen)
l_clone_star c =
    case to_list c of
      [Atom k,Atom n,Atom u] ->
         let k' = ugen_to_int "CLONE-K" k
             n' = ugen_to_int "CLONE-N" n
         in return (Atom (Protect.uclone (const False) k' n' u))
      _ -> Monad.throwError ("clone*: " ++ show c)

l_play_at_star :: Cell UGen -> VM UGen (Cell UGen)
l_play_at_star c =
    case to_list c of
     [_,Atom u,Atom nid,Atom act,Atom grp] ->
         let nid' = ugen_to_int "PLAY-AT: NID" nid
             act' = ugen_to_int "PLAY-AT: ACT" act
             grp' = ugen_to_int "PLAY-AT: GRP" grp
         in lift_io (withSC3 (play_at (nid',toEnum act',grp',[]) u)) >>
            return Nil
     _ -> Monad.throwError ("play-at*: " ++ show c)

l_thread_sleep :: Cell UGen -> VM UGen (Cell UGen)
l_thread_sleep c = do
    u <- atom_err c
    liftIO (pauseThread (ugen_to_double "pause" u))
    return Nil

cell_to_datum :: Cell UGen -> VM a Datum
cell_to_datum c =
    case c of
      Symbol str -> return (string str)
      String str -> return (string str)
      Atom (Constant_U (Constant n)) -> return (float n)
      _ -> Monad.throwError ("CELL-TO-DATUM: " ++ show c)

cell_to_message :: Cell UGen -> VM a Message
cell_to_message c =
    case to_list_m c of
      Just (String addr : l) -> mapM cell_to_datum l >>= \l' -> return (Message addr l')
      _ -> Monad.throwError ("CELL-TO-MESSAGE: " ++ show c)

l_async_star :: Cell UGen -> VM a (Cell a)
l_async_star c = cell_to_message c >>= \c' -> lift_io (withSC3 (void (async c')))

l_send_star :: Cell UGen -> VM a (Cell a)
l_send_star c = cell_to_message c >>= \c' -> lift_io (withSC3 (void (sendMessage c')))

ugen_dict :: Dict UGen
ugen_dict =
    Map.fromList
    [("number?",Fun l_is_number)
    ,("string?",Fun (\c -> case c of {String _ -> l_true; _ -> l_false}))
    ,("symbol?",Fun (\c -> case c of {Symbol _ -> l_true; _ -> l_false}))
    ,("procedure?",Fun l_is_procedure)
    ,("mk-ctl",Proc l_mk_ctl)
    ,("mk-ugen",Proc l_mk_ugen)
    ,("clone*",Proc l_clone_star)
    ,("make-mce",Proc (\c -> fmap (Atom . mce) (mapM atom_err (to_list c))))
    ,("mce-channels",Proc (\c -> fmap (from_list . map Atom . mceChannels) (atom_err c)))
    ,("make-mrg*",Proc (\c -> fmap (Atom . mrg) (mapM atom_err (to_list c))))
    ,("show-graph",Proc (\c -> atom_err c >>= \u -> lift_io (Dot.draw (out 0 u))))
    ,("play-at*",Proc l_play_at_star)
    ,("reset*",Proc (\_ -> lift_io (withSC3 reset)))
    ,("thread-sleep",Proc l_thread_sleep)
    ,("utcr",Proc (\_ -> liftIO time >>= return . Atom . constant))
    ,("display-server-status",Proc (\_ -> lift_io (withSC3 serverStatus >>= mapM_ putStrLn)))
    ,("async*",Proc l_async_star)
    ,("send*",Proc l_send_star)
    ,("unrand",Proc (\c -> atom_err c >>= \u -> return (Atom (ugen_optimise_ir_rand u))))]

main :: IO ()
main = do
  putStrLn "HSC3-LISP"
  env <- gen_toplevel (Map.unions [core_dict,ugen_dict]) :: IO (Env UGen)
  let lib = ["stdlib.lisp"
            ,"scheme.lisp"
            ,"rhs.lisp" -- sw/rhs
            ,"rsc3.prereq.lisp"
            ,"ugen.lisp" -- sw/rsc3
            ,"hsc3.lisp"
            ,"rsc3.lisp" -- sw/rsc3
            ,"alias.lisp"
            ]
  a <- getArgs
  repl env (load_files (lib ++ a))
