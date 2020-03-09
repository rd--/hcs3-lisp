import qualified Data.Map as M {- containers -}
import Data.Ratio {- base -}

import Lisp
import Lisp.Type
import Rational

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
