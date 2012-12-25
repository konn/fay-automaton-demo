{-# LANGUAGE NoImplicitPrelude #-}
module MyPrelude where
import Language.Fay.FFI
import Language.Fay.Prelude

--------------------------------------------------------------
-- Utility functions (missing Prelude functions)
--------------------------------------------------------------
fromJust :: Maybe t -> t
fromJust (Just a) = a
fromJust _ = error "fromJust"

mapM :: (a -> Fay b) -> [a] -> Fay [b]
mapM m (x:xs) = m x >>= (\mx -> mapM m xs >>= (\mxs -> return (mx:mxs)))
mapM _ [] = return []

forM :: [a] -> (a -> Fay b) -> Fay [b]
forM = flip mapM

mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
 let rs = mapMaybe f xs in
 case f x of
  Nothing -> rs
  Just r  -> r:rs

groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []           =  []
groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                           where
                             ans = span (eq x) xs
                             ys = fst ans
                             zs = snd ans

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:xs)
    | x == y    = xs
    | otherwise = y : delete x xs

(<=<) :: (a -> Fay b) -> (t -> Fay a) -> t -> Fay b
(f <=< g) a = g a >>= f

parseInt :: String -> Int
parseInt = ffi "parseInt(%1)"
