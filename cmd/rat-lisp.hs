import qualified Data.Map as M {- containers -}
import Data.Ratio {- base -}

import Sound.SC3.Lisp {- hsc3-lisp -}
import Sound.SC3.Lisp.Type {- hsc3-lisp -}

-- * RATIONAL

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

rat_pp :: (Show i,Integral i) => Ratio i -> String
rat_pp r =
    let n = numerator r
        d = denominator r
    in if d == 1 then show n else concat [show n,"/",show d]

-- * LISP-TY

instance (Show a,Integral a) => Lisp_Ty (Ratio a) where
    ty_show = rat_pp
    ty_to_int = floor
    ty_from_bool t = if t then 1 else 0

-- * NUM / FLOAT

(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap . fmap

map_atom :: Lisp_Ty a => (a -> a) -> Cell a -> Cell a
map_atom f c = maybe (Error ("NOT-ATOM: " ++ show c)) (Atom . f) (atom c)

lift_uop :: Lisp_Ty a => (a -> a) -> Cell a
lift_uop f = Fun (map_atom f)

lift_binop :: Lisp_Ty a => (a -> a -> a) -> Cell a
lift_binop f =
    let g p q = case (p,q) of
                  (Just p',Just q') -> Atom (f p' q')
                  _ -> Error "BINOP: NOT-ATOM?"
    in Fun (\lhs -> Fun (\rhs -> g (atom lhs) (atom rhs)))

rat_dict :: Lisp_Ty a => Dict a
rat_dict =
    M.fromList
    [("+",lift_binop (+))
    ,("*",lift_binop (*))
    ,("-",lift_binop (-))
    ,("/",lift_binop (/))
    ,("<",lift_binop (ty_from_bool .: (<)))
    ,(">",lift_binop (ty_from_bool .: (>)))
    ,("<=",lift_binop (ty_from_bool .: (<=)))
    ,(">=",lift_binop (ty_from_bool .: (>=)))
    ,("negate",lift_uop negate)
    ,("recip",lift_uop recip)]

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
  env <- gen_toplevel (M.union core_dict rat_dict) :: IO (Env Rational)
  repl env (load_files ["stdlib.lisp","rhs.lisp"])
