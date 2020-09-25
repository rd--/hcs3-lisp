module Sound.SC3.Lisp.OSC where

import qualified Data.ByteString.Lazy as Lazy {- bytestring -}

import qualified Language.Scheme.Types as S {- husk-scheme -}

import Sound.OSC {- hosc -}

{-
> map datum_to_lisp [Int32 0,Int64 0,Float 0,Double 0,TimeStamp 0]
> map datum_to_lisp [ASCII_String (ascii "str"),Blob (blob_pack [0,1,2]),Midi (MIDI 0 1 2 3)]
-}
datum_to_lisp :: Datum -> S.LispVal
datum_to_lisp d =
  case d of
    Int32 x -> S.Number (fromIntegral x)
    Int64 x -> S.Number (fromIntegral x)
    Float x -> S.Float (realToFrac x)
    Double x -> S.Float x
    ASCII_String x -> S.String (ascii_to_string x)
    Blob x -> S.ByteVector (Lazy.toStrict x)
    TimeStamp x -> S.Float x
    Midi (MIDI m1 m2 m3 m4) -> S.List (S.Atom "midi" : map (S.Number . fromIntegral) [m1,m2,m3,m4])

-- > message_to_lisp False (Message "/c_set" [Int32 0,Float 1])
message_to_lisp :: Bool -> Message -> S.LispVal
message_to_lisp ty (Message a d) =
  if ty
  then S.List (S.Atom a : S.String (',' : map datum_tag d) : map datum_to_lisp d)
  else S.List (S.Atom a : map datum_to_lisp d)

-- > bundle_to_lisp False (Bundle 0 [Message "/c_set" [Int32 0,Float 1],Message "/nil" []])
bundle_to_lisp :: Bool -> Bundle -> S.LispVal
bundle_to_lisp ty (Bundle t m) = S.List (S.Atom "bundle" : S.Float t : map (message_to_lisp ty) m)
