-- | Environment
module Sound.SC3.Lisp.Env where

import Control.Monad.ST {- base -}
import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}
import Data.IORef {- base -}
import Data.Maybe {- base -}
import Data.STRef {- base -}

import qualified Control.Monad.State as Monad {- mtl -}
import qualified Control.Monad.Except as Monad {- mtl -}
import qualified Data.Map as Map {- containers -}

-- | Dictionary key.
type Name = String

-- | Dictionary.
type Dict t = Map.Map Name t

-- | Enviroment, either a Frame or a Toplevel, f is the reference type, t is the value type.
data Env_ f t = Frame (f (Dict t)) (Env_ f t)
              | Toplevel (f (Dict t))

-- | State monad wrapped in Execption monad.
type EnvMonad_ f m t r = Monad.ExceptT Name (Monad.StateT (Env_ f t) m) r

-- | Env_ at IORef, there could be but is not an STRef variant
type Env t = Env_ IORef t

-- | EnvMonad_ at IORef, there could be but is not an STRef variant
type EnvMonad m t r = EnvMonad_ IORef m t r

-- | 'newIORef' of 'readIORef'
ioref_copy :: IORef a -> IO (IORef a)
ioref_copy r = readIORef r >>= newIORef

-- | 'newSTRef' of 'readSTRef'
stref_copy :: STRef s a -> ST s (STRef s a)
stref_copy r = readSTRef r >>= newSTRef

env_copy_ :: Monad m => (f (Dict t) -> m (f (Dict t))) -> Env_ f t -> m (Env_ f t)
env_copy_ copy e =
    case e of
      Frame f e' -> do f' <- copy f
                       e'' <- env_copy_ copy e'
                       return (Frame f' e'')
      Toplevel d -> do d' <- copy d
                       return (Toplevel d')

-- | Copy environment.
env_copy :: Env a -> IO (Env a)
env_copy = env_copy_ ioref_copy

-- | Print environment.
env_print :: Show t => Env t -> IO ()
env_print e =
    case e of
      Frame f e' -> readIORef f >>= print >> env_print e'
      Toplevel d -> readIORef d >>= print

-- | New empty environment.
env_empty :: IO (Env t)
env_empty = fmap Toplevel (newIORef Map.empty)

-- | New environment from 'Dict'.
env_gen_toplevel :: Dict a -> IO (Env a)
env_gen_toplevel = fmap Toplevel . newIORef

-- | Lookup value in environment, maybe variant.
env_lookup_m :: MonadIO m => Name -> Env t -> m (Maybe t)
env_lookup_m w e =
    case e of
      Frame f e' -> do
             d <- liftIO (readIORef f)
             case Map.lookup w d of
               Just r -> return (Just r)
               Nothing -> env_lookup_m w e'
      Toplevel d -> do
             d' <- liftIO (readIORef d)
             case Map.lookup w d' of
               Just r -> return (Just r)
               Nothing -> return Nothing

-- | Lookup with default.
env_lookup_def :: MonadIO m => Name -> Env t -> t -> EnvMonad m t t
env_lookup_def w e t = do
  r <- env_lookup_m w e
  return (fromMaybe t r)

-- | Lookup value in environment, error variant.
env_lookup :: MonadIO m => Name -> Env t -> EnvMonad m t t
env_lookup w e = do
  r <- env_lookup_m w e
  case r of
    Nothing -> throwError ("env_lookup: " ++ w)
    Just c -> return c

-- | Extend environment by adding a frame given as an association list.
env_add_frame :: [(Name,t)] -> Env t -> IO (Env t)
env_add_frame d e = do
  f <- newIORef (Map.fromList d)
  return (Frame f e)

-- | Delete current frame from environment, error if at toplevel.
env_del_frame :: MonadIO m => Env t -> EnvMonad m t (Env t)
env_del_frame e =
  case e of
    Frame _ e' -> return e'
    Toplevel _ -> throwError "env_del_frame: at toplevel?"

-- | Set value in environment.  If no entry exists for Name create an entry at the top level.
env_set :: Env t -> Name -> t -> IO ()
env_set e nm c =
    case e of
      Frame f e' -> do
             d <- liftIO (readIORef f)
             if Map.member nm d then writeIORef f (Map.insert nm c d) else env_set e' nm c
      Toplevel d -> modifyIORef d (Map.insert nm c)

-- | Lookup value or error, apply f, set value to result, return result.
env_alter :: MonadIO m => Env t -> Name -> (t -> t) -> EnvMonad m t t
env_alter e nm f = do
  v <- env_lookup nm e
  let r = f v
  liftIO (env_set e nm r)
  return r

-- | Lookup value or default, apply f, set value to result, return result.
env_alter_def :: MonadIO m => Env t -> Name -> t -> (t -> t) -> EnvMonad m t t
env_alter_def e nm t f = do
  v <- env_lookup_def nm e t
  let r = f v
  liftIO (env_set e nm r)
  return r
