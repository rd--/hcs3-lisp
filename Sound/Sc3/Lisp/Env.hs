{-# Language FlexibleContexts #-}

-- | Environment
module Sound.Sc3.Lisp.Env where

import Control.Monad.IO.Class {- base -}
import Data.IORef {- base -}
import Data.Maybe {- base -}

import qualified Control.Monad.Except as Except {- mtl -}
import qualified Data.Map as Map {- containers -}

import Interpreter.Som.Dict {- stsc3-som -}
import Interpreter.Som.DictRef {- stsc3-som -}
import Interpreter.Som.Ref {- stsc3-som -}

{- | Enviroment, a dictionary reference with an optional pointer to a parent environment.
     k is the key type, v is the value type.
     Assign mutates the reference for the frame the variable is located at.
-}
data Env k v = Env (DictRef k v) (Maybe (Env k v)) deriving (Eq)

-- | List of all keys at Env as descending frames
envKeys :: MonadIO m => Env k v -> m [[k]]
envKeys (Env r p) = dictRefKeys r >>= \dk -> maybe (return []) envKeys p >>= \pk -> return (dk : pk)

-- | Fetch Toplevel of Env.
envToplevel :: Env k v -> Env k v
envToplevel e =
  case e of
    Env _ Nothing -> e
    Env _ (Just p) -> envToplevel p

-- | Copy environment.
envCopy :: (MonadIO m, Ord k) => Env k v -> m (Env k v)
envCopy (Env r p) = do
  r' <- dictRefCopy r
  p' <- maybe (return Nothing) (fmap Just . envCopy) p
  return (Env r' p')

-- | Print environment.
envPrint :: (Show k, Show v) => Env k v -> IO ()
envPrint (Env r p) = do
  deRef r >>= dictPrint
  maybe (return ()) envPrint p

-- | New environment from 'Dict'.
envNewFrom :: MonadIO m => Dict k v -> m (Env k v)
envNewFrom d = toRef d >>= \r -> return (Env r Nothing)

envNewFromList :: (MonadIO m, Ord k) => [Dict k v] -> m (Env k v)
envNewFromList l = envNewFrom (dictMergeList l)

-- | New empty environment.
envEmpty :: MonadIO m => m (Env k v)
envEmpty = envNewFrom Map.empty

-- | Lookup value only in current frame (do not recurse into parent environment).
envLookupCurrentFrameMaybe :: (MonadIO m, Ord k) => k -> Env k v -> m (Maybe v)
envLookupCurrentFrameMaybe w (Env r _) = dictRefLookup r w

-- | Lookup value in environment, maybe variant.
envLookupMaybe :: (MonadIO m, Ord k) => k -> Env k v -> m (Maybe v)
envLookupMaybe w (Env r p) = do
  x <- dictRefLookup r w
  case (x,p) of
    (Nothing,Just e) -> envLookupMaybe w e
    _ -> return x

-- | Lookup with default.
envLookupWithDefault :: (MonadIO m, Ord k) => k -> Env k v -> v -> m v
envLookupWithDefault k e d = do
  r <- envLookupMaybe k e
  return (fromMaybe d r)

-- | Lookup value in environment, error variant.
envLookupError :: (MonadIO m, Except.MonadError String m, Ord k) => String -> k -> Env k v -> m v
envLookupError msg k e = do
  r <- envLookupMaybe k e
  case r of
    Nothing -> Except.throwError msg
    Just c -> return c

-- | Lookup value in environment, error variant.
envLookup :: (MonadIO m, Except.MonadError String m, Ord k, Show k) => k -> Env k v -> m v
envLookup k = envLookupError ("envLookup: no key: " ++ show k) k

-- | Run 'envLookup' at 'envToplevel'
envLookupToplevel :: (MonadIO m, Except.MonadError String m, Ord k, Show k) => k -> Env k v -> m v
envLookupToplevel k e = envLookup k (envToplevel e)

-- | Extend Env by adding a frame.
envAddFrame :: MonadIO m => Dict k v -> Env k v -> m (Env k v)
envAddFrame d e = liftIO (newIORef d) >>= \r -> return (Env r (Just e))

-- | Extend environment by adding a frame given as an association list.
envAddFrameFromList :: (MonadIO m,Ord k) => [(k,v)] -> Env k v -> m (Env k v)
envAddFrameFromList l e = dictFromList l >>= \d -> envAddFrame d e

-- | Delete current frame if one exists.
envDeleteFrameMaybe :: Env k v -> Maybe (Env k v)
envDeleteFrameMaybe (Env _ p) = p

-- | Delete current frame from environment, error if at toplevel.
envDeleteFrame :: (MonadIO m, Except.MonadError String m) => Env k v -> m (Env k v)
envDeleteFrame (Env _ p) = maybe (Except.throwError "envDeleteFrame") return p

{- | Set value in environment.
     If no entry exists create an entry at the top level.
     Return value set.
-}
envSet :: (MonadIO m, Ord k) => Env k v -> k -> v -> m v
envSet (Env r p) key value =
  case p of
    Nothing -> dictRefInsert r key value >> return value
    Just e -> do
      exists <- dictRefAssign r key value
      if exists then return value else envSet e key value

-- | Run 'envSet' at 'envToplevel'.
envSetToplevel :: (MonadIO m, Ord k) => Env k v -> k -> v -> m v
envSetToplevel e nm c = envSet (envToplevel e) nm c

-- | Lookup value or error, apply f, set value to result, return result.
envAlter :: (MonadIO m, Except.MonadError String m, Ord k, Show k) => Env k v -> k -> (v -> v) -> m v
envAlter e nm f = do
  v <- envLookup nm e
  let r = f v
  liftIO (envSet e nm r)

-- | Lookup value or default, apply f, set value to result, return result.
envAlterWithDefault :: (MonadIO m, Ord k) => Env k v -> k -> v -> (v -> v) -> m v
envAlterWithDefault e nm t f = do
  v <- envLookupWithDefault nm e t
  let r = f v
  liftIO (envSet e nm r)
