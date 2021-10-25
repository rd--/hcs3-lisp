-- | Translation between Open Sound Control packets and S-Expressions (ie. packet_to_lisp and lisp_to_packet).
module Sound.SC3.Lisp.Osc where

import Data.Word {- base -}

import qualified Data.Array as Array {- array -}
import qualified Data.ByteString as Strict {- bytestring -}
import qualified Data.ByteString.Lazy as Lazy {- bytestring -}

import qualified Language.Scheme.Parser as S {- husk-scheme -}
import qualified Language.Scheme.Types as S {- husk-scheme -}

import Sound.OSC {- hosc -}

import qualified Sound.SC3.Lisp.Parse.Ethier as S {- hsc3-lisp -}

-- * UTIL

-- | cons p q
s_cons :: S.LispVal -> S.LispVal -> S.LispVal
s_cons p q = S.DottedList [p] q

-- | [p,q,r] to #(p q r)
s_list_to_vector :: [S.LispVal] -> S.LispVal
s_list_to_vector l = S.Vector (Array.listArray (0,length l - 1) l)

-- | Convert 'S.Number' to 'Word8', 'error' if not a number or out of range.
s_word8 :: S.LispVal -> Word8
s_word8 l =
  case l of
    S.Number x -> if x >= 0 && x <= 255 then fromIntegral x else error "s_word8: out-of-range"
    _ -> error "s_word8: not-a-number"

s_bytevector_to_vector :: Lazy.ByteString -> S.LispVal
s_bytevector_to_vector = s_list_to_vector . map (S.Number . fromIntegral) . Lazy.unpack

list_to_midi :: [Word8] -> MIDI
list_to_midi l =
  case l of
    [a,b,c,d] -> MIDI a b c d
    _ -> error "list_to_midi"

bytestring_to_midi :: Strict.ByteString -> MIDI
bytestring_to_midi x =
  if Strict.length x == 4
  then list_to_midi (Strict.unpack x)
  else error "bytevector_to_midi"

-- * TO-LISP

-- | 'MIDI' to (midi . #(i j k l))
midi_to_lisp :: Bool -> MIDI -> S.LispVal
midi_to_lisp u8 (MIDI m1 m2 m3 m4) =
  let v = if u8
          then S.ByteVector (Lazy.toStrict (Lazy.pack [m1,m2,m3,m4]))
          else s_list_to_vector (map (S.Number . fromIntegral) [m1,m2,m3,m4])
  in s_cons (S.Atom "midi") v

{- | Convert 'Datum' to 'S.LispVal'.
     If /u8/ is 'True' then encode 'Blob' and 'Midi' data using byte-vectors, else use vectors.

> d = [int32 0,int64 0,float 0,double 0,TimeStamp 0]
> l = map (datum_to_lisp undefined) d
> map (lisp_to_datum (int32,float)) l == [int32 0,int32 0,float 0,float 0,TimeStamp 0]

> d = [string "str",blob [0,1,2],midi (0,1,2,3)]
> l = map (datum_to_lisp False) d
> map (lisp_to_datum (int32,float)) l == d
-}
datum_to_lisp :: Bool -> Datum -> S.LispVal
datum_to_lisp u8 d =
  case d of
    Int32 x -> S.Number (fromIntegral x)
    Int64 x -> S.Number (fromIntegral x)
    Float x -> S.Float (realToFrac x)
    Double x -> S.Float x
    ASCII_String x -> S.String (ascii_to_string x)
    Blob x -> (if u8 then S.ByteVector . Lazy.toStrict else s_bytevector_to_vector) x
    TimeStamp x -> s_cons (S.Atom "timestamp") (S.Float x)
    Midi x -> midi_to_lisp u8 x

message_to_lisp :: (Bool,Bool) -> Message -> S.LispVal
message_to_lisp (ty,u8) (Message a d) =
  if ty
  then S.List (s_cons (S.String a) (S.String (',' : map datum_tag d)) : map (datum_to_lisp u8) d)
  else S.List (S.String a : map (datum_to_lisp u8) d)

bundle_to_lisp :: (Bool,Bool) -> Bundle -> S.LispVal
bundle_to_lisp opt (Bundle t m) = S.List (S.String "#bundle" : S.Float t : map (message_to_lisp opt) m)

-- * FROM-LISP

-- | Convert 'S.LispVal' to 'Datum' given functions to determine encodings for integers and floats.
lisp_to_datum :: (Integer -> Datum,Double -> Datum) -> S.LispVal -> Datum
lisp_to_datum (i,f) l =
  case l of
    S.Number x -> i x
    S.Float x -> f x
    S.ByteVector x -> Blob (Lazy.fromStrict x)
    S.Vector x -> Blob (Lazy.pack (map s_word8 (Array.elems x)))
    S.String x -> string x
    S.DottedList [S.Atom "midi"] (S.Vector x) -> Midi (list_to_midi (map s_word8 (Array.elems x)))
    S.DottedList [S.Atom "midi"] (S.ByteVector x) -> Midi (bytestring_to_midi x)
    S.DottedList [S.Atom "timestamp"] (S.Float x) -> TimeStamp x
    _ -> error "lisp_to_datum?"

{- | Convert 'S.LispVal' to 'Message', inverse of 'message_to_lisp'

> m = Message "/m" [int32 0,float 1,blob [1,2,3],midi (1,2,3,4)]
> l = message_to_lisp (False,False) m
> lisp_to_message (int32,float) l == m
-}
lisp_to_message :: (Integer -> Datum,Double -> Datum) -> S.LispVal -> Message
lisp_to_message opt l =
  case l of
    S.List (S.String a : d) -> Message a (map (lisp_to_datum opt) d)
    S.List (S.DottedList [S.String a] _ : d) -> Message a (map (lisp_to_datum opt) d)
    _ -> error "lisp_to_message"

{- | Convert 'S.LispVal' to 'Bundle', inverse of 'bundle_to_lisp'.

> b = Bundle 0 [Message "/c_set" [Int32 0,Float 1],Message "/nil" []]
> l = bundle_to_lisp (True,True) b
> lisp_to_bundle (int32,float) l == b
-}
lisp_to_bundle :: (Integer -> Datum,Double -> Datum) -> S.LispVal -> Bundle
lisp_to_bundle opt l =
  case l of
    S.List (S.String "#bundle" : S.Float t : m) -> Bundle t (map (lisp_to_message opt) m)
    _ -> error "lisp_to_bundle"

-- | A list where the first element is a string or a string pair, and the first letters are / and ,.
lisp_is_message :: S.LispVal -> Bool
lisp_is_message l =
  case l of
    S.List (S.String ty : _) -> head ty == '/'
    S.List (S.DottedList [S.String ty] (S.String sig) : _) -> head ty == '/' && head sig == ','
    _ -> False

-- | A list where the first element is the string #bundle, the second a float and the rest messages.
lisp_is_bundle :: S.LispVal -> Bool
lisp_is_bundle l =
  case l of
    S.List (S.String "#bundle" : S.Float _ : m) -> all lisp_is_message m
    _ -> False

-- | Translate from s-expression 'S.LispVal' to OSC 'Packet'.
lisp_to_packet :: (Integer -> Datum,Double -> Datum) -> S.LispVal -> Packet
lisp_to_packet opt l =
  if lisp_is_bundle l
  then Packet_Bundle (lisp_to_bundle opt l)
  else if lisp_is_message l
       then Packet_Message (lisp_to_message opt l)
       else error "lisp_to_packet"

-- | Translate OSC 'Packet' to 'S.LispVal'.
packet_to_lisp :: (Bool, Bool) -> Packet -> S.LispVal
packet_to_lisp opt pkt =
  case pkt of
    Packet_Bundle x -> bundle_to_lisp opt x
    Packet_Message x -> message_to_lisp opt x

-- | 'S.sexp_show' of 'packet_to_lisp'
lisp_print_packet :: (Bool, Bool) -> Packet -> String
lisp_print_packet opt = S.sexp_show . packet_to_lisp opt


{- | 'lisp_to_packet' of 'S.readExpr'

> b = Bundle 0 [Message "/c_set" [Int32 0,Float 1],Message "/nil" []]
> p = Packet_Bundle b
> s = lisp_print_packet (False,False) p
> s = lisp_print_packet (True,False) p
> putStrLn s
> lisp_parse_packet (int32,float) s == p
-}
lisp_parse_packet :: (Integer -> Datum,Double -> Datum) -> String -> Packet
lisp_parse_packet opt txt =
  case S.readExpr txt of
    Left err -> error (show err)
    Right x -> lisp_to_packet opt x
