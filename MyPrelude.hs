{-# LANGUAGE NoImplicitPrelude #-}
module MyPrelude where
import Language.Fay.FFI
import Language.Fay.Prelude

--------------------------------------------------------------
-- Utility functions (missing Prelude functions)
--------------------------------------------------------------
on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on op f x y = f x `op` f y

slice :: Int -> [a] -> [[a]]
slice len = step id
  where
    step acc [] = acc []
    step acc xs =
        case splitAt len xs of
          (ts, rest) -> step (acc . (ts:)) rest

maximumBy               :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ []          =  error "List.maximumBy: empty list"
maximumBy cmp xs        =  foldl1 maxBy xs
                        where
                           maxBy x y = case cmp x y of
                                       GT -> x
                                       _  -> y

nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq l              = nubBy' l []
  where
    nubBy' [] _         = []
    nubBy' (y:ys) xs
       | elem_by eq y xs = nubBy' ys xs
       | otherwise       = y : nubBy' ys (y:xs)

elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
elem_by _  _ []         =  False
elem_by eq y (x:xs)     =  y `eq` x || elem_by eq y xs

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


groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy eq = sub id
  where
    sub acc [] = acc []
    sub acc xs@(x:_) = sub (acc . (cs:)) rest
      where
        ans = span (eq x) xs
        cs = fst ans
        rest = snd ans

delete :: Eq a => a -> [a] -> [a]
delete = deleteBy (==)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy eq x (y:xs)
    | x `eq` y    = xs
    | otherwise = y : deleteBy eq x xs

(<=<) :: (a -> Fay b) -> (t -> Fay a) -> t -> Fay b
(f <=< g) a = g a >>= f

partition :: (t -> Bool) -> [t] -> ([t], [t])
partition p xs = foldr (select p) ([],[]) xs

select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
select p x (ts, fs) | p x       = ts `seq` fs `seq` (x:ts,fs)
                    | otherwise = ts `seq` fs `seq` (ts, x:fs)


parseInt :: String -> Int
parseInt = ffi "parseInt(%1)"
