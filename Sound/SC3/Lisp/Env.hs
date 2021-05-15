-- | Environment
module Sound.SC3.Lisp.Env where

import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}
import Data.IORef {- base -}

import qualified Control.Monad.State as Monad {- mtl -}
import qualified Control.Monad.Except as Monad {- mtl -}
import qualified Data.Map as Map {- containers -}

type Dict t = Map.Map String t

data Env t = Frame (IORef (String,t)) (Env t)
           | Toplevel (IORef (Dict t))

-- data ST a = ST {st_threads :: Map.Map Int ThreadId,st_env :: Env a}
type VM t r = Monad.ExceptT String (Monad.StateT (Env t) IO) r

-- | 'newIORef' of 'readIORef'
ioref_cpy :: IORef a -> IO (IORef a)
ioref_cpy r = readIORef r >>= newIORef

env_copy :: Env a -> IO (Env a)
env_copy e =
    case e of
      Frame f e' -> do f' <- ioref_cpy f
                       e'' <- env_copy e'
                       return (Frame f' e'')
      Toplevel d -> do d' <- ioref_cpy d
                       return (Toplevel d')

env_print :: Show t => Env t -> IO ()
env_print e =
    case e of
      Frame f e' -> readIORef f >>= print >> env_print e'
      Toplevel d -> readIORef d >>= print

env_empty :: IO (Env t)
env_empty = do
  d <- newIORef Map.empty
  return (Toplevel d)

env_lookup_m :: String -> Env t -> VM t (Maybe t)
env_lookup_m w e =
    case e of
      Frame f e' -> do
             (k,v) <- liftIO (readIORef f)
             if w == k then return (Just v) else env_lookup_m w e'
      Toplevel d -> do
             d' <- liftIO (readIORef d)
             case Map.lookup w d' of
               Just r -> return (Just r)
               Nothing -> return Nothing

trace :: Show t => Int -> String -> t -> VM a ()
trace lvl msg val = when (lvl < 3) (liftIO (putStrLn ("trace: " ++ msg ++ ": " ++ show val)))

env_lookup :: Show t => String -> Env t -> VM t t
env_lookup w e = do
  r <- env_lookup_m w e
  trace 5 "env_lookup" (w,r)
  case r of
    Nothing -> throwError ("env-lookup: " ++ w)
    Just c -> return c

env_add_frame :: String -> t -> Env t -> IO (Env t)
env_add_frame k v e = do
  f <- newIORef (k,v)
  return (Frame f e)

env_set :: Env t -> String -> t -> IO ()
env_set e nm c =
    case e of
      Frame f e' -> do
             (k,_) <- liftIO (readIORef f)
             if nm == k then writeIORef f (nm,c) else env_set e' nm c
      Toplevel d -> modifyIORef d (Map.insert nm c)

gen_toplevel :: Dict a -> IO (Env a)
gen_toplevel = fmap Toplevel . newIORef

