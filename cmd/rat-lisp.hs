import qualified Data.Map as M {- containers -}
import Data.Ratio {- base -}

import qualified Interpreter.Som.Dict as Dict {- stsc3-som -}

import qualified Language.Sc3.Lisp.Env as Env {- hsc3-lisp -}
import Language.Sc3.Lisp.Interpreter {- hsc3-lisp -}
import Language.Sc3.Lisp.Type {- hsc3-lisp -}

-- * Rational

{-
import Safe {- safe -}
import qualified Text.Read as R {- base -}

sep :: Eq a => a -> [a] -> ([a],[a])
sep c s = let (lhs,rhs) = break (== c) s in (lhs,tailDef [] rhs)

bimap1 :: (t -> t1) -> (t, t) -> (t1, t1)
bimap1 f (p,q) = (f p,f q)

parse_int :: String -> Maybe Integer
parse_int = R.readMaybe

parse_rat :: String -> Maybe Rational
parse_rat s =
    case bimap1 parse_int (sep '/' s) of
      (Just n,Just d) -> Just (n % d)
      _ ->
          case parse_int s of
            Just i -> Just (fromInteger i)
            Nothing -> fmap realToFrac (R.readMaybe s :: Maybe Double)
-}

rat_pp :: (Show i, Integral i) => Ratio i -> String
rat_pp r =
  let n = numerator r
      d = denominator r
  in if d == 1 then show n else concat [show n, "/", show d]

-- * Lisp-Ty

instance (Show a, Integral a) => Lisp_Ty (Ratio a) where
  ty_show = rat_pp
  ty_to_int = floor
  ty_from_bool t = if t then 1 else 0

-- * Num / Float

(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap . fmap

map_atom :: Lisp_Ty a => (a -> a) -> Expr a -> Expr a
map_atom f c = maybe (Error ("NOT-ATOM: " ++ show c)) (Atom . f) (atom c)

lift_uop :: Lisp_Ty a => (a -> a) -> Expr a
lift_uop f = Fun (map_atom f)

lift_binop :: Lisp_Ty a => (a -> a -> a) -> Expr a
lift_binop f =
  let g p q = case (p, q) of
        (Just p', Just q') -> Atom (f p' q')
        _ -> Error "BINOP: NOT-ATOM?"
  in Fun (\lhs -> Fun (\rhs -> g (atom lhs) (atom rhs)))

rat_dict :: IO (Dict.Dict String (Expr Rational))
rat_dict =
  Dict.dictFromList
    [ ("+", lift_binop (+))
    , ("*", lift_binop (*))
    , ("-", lift_binop (-))
    , ("/", lift_binop (/))
    , ("<", lift_binop (ty_from_bool .: (<)))
    , (">", lift_binop (ty_from_bool .: (>)))
    , ("<=", lift_binop (ty_from_bool .: (<=)))
    , (">=", lift_binop (ty_from_bool .: (>=)))
    , ("negate", lift_uop negate)
    , ("recip", lift_uop recip)
    ]

{-
float_dict :: (Lisp_Ty a,Floating a) => Dict a
float_dict =
    M.fromList
    [("sin",lift_uop sin)
    ,("cos",lift_uop cos)]
-}

main :: IO ()
main = do
  putStrLn "RAT-LISP"
  env <- sequence [core_dict, rat_dict] >>= Env.envNewFromList :: IO (Env.Env String (Expr Rational))
  repl_init env (load_files ["stdlib.scm", "rhs.prereq.scm", "rhs.scm"])
