-- | Environment
module Sound.SC3.Lisp.Env where

import Control.Monad.IO.Class {- base -}
import Data.IORef {- base -}
import Data.Maybe {- base -}

import qualified Control.Monad.State as State {- mtl -}
import qualified Control.Monad.Except as Except {- mtl -}
import qualified Data.Map as Map {- containers -}

-- | Dictionary.
type Dict k v = Map.Map k v

{- | Enviroment, either a Frame or a Toplevel.
     k is the key type, v is the value type.
     The implementation uses IORef cells for each Dict.
     Set mutates the reference for the frame the variable is located at.
-}
data Env k v
  = Frame (IORef (Dict k v)) (Env k v)
  | Toplevel (IORef (Dict k v))

-- | State monad wrapped in Exception monad.
type EnvMonad m k v r = Except.ExceptT String (State.StateT (Env k v) m) r

-- | 'newIORef' of 'readIORef'
copyIORef :: MonadIO m => IORef a -> m (IORef a)
copyIORef r = liftIO (readIORef r >>= newIORef)

-- | Copy environment.
envCopy :: MonadIO m => Env k v -> m (Env k v)
envCopy e =
  case e of
    Frame d e' ->
      do d' <- copyIORef d
         e'' <- envCopy e'
         return (Frame d' e'')
    Toplevel d ->
      do d' <- copyIORef d
         return (Toplevel d')

-- | Print environment.
envPrint :: (Show k, Show v) => Env k v -> IO ()
envPrint e =
    case e of
      Frame f e' -> readIORef f >>= print >> envPrint e'
      Toplevel d -> readIORef d >>= print

-- | New environment from 'Dict'.
envNewFrom :: MonadIO m => Dict k v -> m (Env k v)
envNewFrom d = fmap Toplevel (liftIO (newIORef d))

-- | New empty environment.
envEmpty :: MonadIO m => m (Env k v)
envEmpty = envNewFrom Map.empty

-- | Lookup value in environment, maybe variant.
envLookupMaybe :: (MonadIO m, Ord k) => k -> Env k v -> m (Maybe v)
envLookupMaybe w e =
  case e of
    Frame f e' ->
      do d <- liftIO (readIORef f)
         case Map.lookup w d of
           Just r -> return (Just r)
           Nothing -> envLookupMaybe w e'
    Toplevel d ->
      do d' <- liftIO (readIORef d)
         case Map.lookup w d' of
           Just r -> return (Just r)
           Nothing -> return Nothing

-- | Lookup with default.
envLookupWithDefault :: (MonadIO m, Ord k) => k -> Env k v -> v -> m v
envLookupWithDefault k e d = do
  r <- envLookupMaybe k e
  return (fromMaybe d r)

-- | Lookup value in environment, error variant.
envLookup :: (MonadIO m, Ord k, Show k) => k -> Env k v -> EnvMonad m k v v
envLookup k e = do
  r <- envLookupMaybe k e
  case r of
    Nothing -> Except.throwError ("envLookup" ++ show k)
    Just c -> return c

-- | Extend Env by adding a frame.
envAddFrame :: MonadIO m => Dict k v -> Env k v -> m (Env k v)
envAddFrame d e = do
  f <- liftIO (newIORef d)
  return (Frame f e)

-- | Extend environment by adding a frame given as an association list.
envAddFrameFromList :: (MonadIO m,Ord k) => [(k,v)] -> Env k v -> m (Env k v)
envAddFrameFromList d e = envAddFrame (Map.fromList d) e

-- | Delete current frame if one exists.
envDeleteFrameMaybe :: Env k v -> Maybe (Env k v)
envDeleteFrameMaybe e =
  case e of
    Frame _ e' -> Just e'
    Toplevel _ -> Nothing

-- | Delete current frame from environment, error if at toplevel.
envDeleteFrame :: MonadIO m => Env k v -> EnvMonad m k v (Env k v)
envDeleteFrame e =
  case e of
    Frame _ e' -> return e'
    Toplevel _ -> Except.throwError "envDeleteFrame: at toplevel?"

-- | Set value in environment.  If no entry exists for Name create an entry at the top level.
envSet :: (MonadIO m, Ord k) => Env k v -> k -> v -> m ()
envSet e nm c =
  case e of
    Frame f e' ->
      do d <- liftIO (readIORef f)
         if Map.member nm d then liftIO (writeIORef f (Map.insert nm c d)) else envSet e' nm c
    Toplevel d -> liftIO (modifyIORef d (Map.insert nm c))

-- | Lookup value or error, apply f, set value to result, return result.
envAlter :: (MonadIO m, Ord k, Show k) => Env k v -> k -> (v -> v) -> EnvMonad m k v v
envAlter e nm f = do
  v <- envLookup nm e
  let r = f v
  liftIO (envSet e nm r)
  return r

-- | Lookup value or default, apply f, set value to result, return result.
envAlterWithDefault :: (MonadIO m, Ord k) => Env k v -> k -> v -> (v -> v) -> m v
envAlterWithDefault e nm t f = do
  v <- envLookupWithDefault nm e t
  let r = f v
  liftIO (envSet e nm r)
  return r
