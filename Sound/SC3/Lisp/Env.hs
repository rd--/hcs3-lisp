-- | Environment
module Sound.SC3.Lisp.Env where

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
copyIORef :: IORef a -> IO (IORef a)
copyIORef r = readIORef r >>= newIORef

-- | Copy environment.
envCopy :: Env k v -> IO (Env k v)
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

-- | New empty environment.
envEmpty :: IO (Env k v)
envEmpty = fmap Toplevel (newIORef Map.empty)

-- | New environment from 'Dict'.
envNewFrom :: Dict k v -> IO (Env k v)
envNewFrom = fmap Toplevel . newIORef

-- | Lookup value in environment, maybe variant.
envLookupMaybe :: (State.MonadIO m,Ord k) => k -> Env k v -> m (Maybe v)
envLookupMaybe w e =
  case e of
    Frame f e' ->
      do d <- State.liftIO (readIORef f)
         case Map.lookup w d of
           Just r -> return (Just r)
           Nothing -> envLookupMaybe w e'
    Toplevel d ->
      do d' <- State.liftIO (readIORef d)
         case Map.lookup w d' of
           Just r -> return (Just r)
           Nothing -> return Nothing

-- | Lookup with default.
envLookupWithDefault :: (State.MonadIO m,Ord k) => k -> Env k v -> v -> EnvMonad m k v v
envLookupWithDefault k e d = do
  r <- envLookupMaybe k e
  return (fromMaybe d r)

-- | Lookup value in environment, error variant.
envLookup :: (State.MonadIO m, Ord k, Show k) => k -> Env k v -> EnvMonad m k v v
envLookup k e = do
  r <- envLookupMaybe k e
  case r of
    Nothing -> Except.throwError ("envLookup" ++ show k)
    Just c -> return c

-- | Extend environment by adding a frame given as an association list.
envAddFrame :: Ord k => [(k,v)] -> Env k v -> IO (Env k v)
envAddFrame d e = do
  f <- newIORef (Map.fromList d)
  return (Frame f e)

-- | Delete current frame from environment, error if at toplevel.
envDeleteFrame :: State.MonadIO m => Env k v -> EnvMonad m k v (Env k v)
envDeleteFrame e =
  case e of
    Frame _ e' -> return e'
    Toplevel _ -> Except.throwError "envDeleteFrame: at toplevel?"

-- | Set value in environment.  If no entry exists for Name create an entry at the top level.
envSet :: Ord k => Env k v -> k -> v -> IO ()
envSet e nm c =
  case e of
    Frame f e' ->
      do d <- State.liftIO (readIORef f)
         if Map.member nm d then writeIORef f (Map.insert nm c d) else envSet e' nm c
    Toplevel d -> modifyIORef d (Map.insert nm c)

-- | Lookup value or error, apply f, set value to result, return result.
envAlter :: (State.MonadIO m, Ord k, Show k) => Env k v -> k -> (v -> v) -> EnvMonad m k v v
envAlter e nm f = do
  v <- envLookup nm e
  let r = f v
  State.liftIO (envSet e nm r)
  return r

-- | Lookup value or default, apply f, set value to result, return result.
envAlterWithDefault :: (State.MonadIO m, Ord k) => Env k v -> k -> v -> (v -> v) -> EnvMonad m k v v
envAlterWithDefault e nm t f = do
  v <- envLookupWithDefault nm e t
  let r = f v
  State.liftIO (envSet e nm r)
  return r
