-- | Environment
module Sound.SC3.Lisp.Env where

import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}
import Data.IORef {- base -}

import qualified Control.Monad.State as Monad {- mtl -}
import qualified Control.Monad.Except as Monad {- mtl -}
import qualified Data.Map as Map {- containers -}

type Name = String

type Dict t = Map.Map Name t

data Env t = Frame (IORef (Dict t)) (Env t)
           | Toplevel (IORef (Dict t))

-- | 'newIORef' of 'readIORef'
ioref_cpy :: IORef a -> IO (IORef a)
ioref_cpy r = readIORef r >>= newIORef

-- | Copy environment.
env_copy :: Env a -> IO (Env a)
env_copy e =
    case e of
      Frame f e' -> do f' <- ioref_cpy f
                       e'' <- env_copy e'
                       return (Frame f' e'')
      Toplevel d -> do d' <- ioref_cpy d
                       return (Toplevel d')

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

-- | Lookup value in environment, error variant.
env_lookup :: Show t => Name -> Env t -> Monad.ExceptT Name (Monad.StateT (Env t) IO) t
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

-- | Set value in environment.  If no entry exists for Name create an entry at the top level.
env_set :: Env t -> Name -> t -> IO ()
env_set e nm c =
    case e of
      Frame f e' -> do
             d <- liftIO (readIORef f)
             if Map.member nm d then writeIORef f (Map.insert nm c d) else env_set e' nm c
      Toplevel d -> modifyIORef d (Map.insert nm c)
