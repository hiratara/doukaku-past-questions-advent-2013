{-# LANGUAGE TupleSections #-}
module Doukaku.Rails (solve) where
import Data.Char (ord, chr)

solve :: String -> String
solve input = go 1 0
  where
    go p d = label p : case move p d of
      Just (p', d') -> go p' d'
      Nothing       -> []
    move p d = fmap (, direct .@. d') (conn p .@. d')
      where
        d' = panel (input !! p) .@. d

label :: Int -> Char
label = chr . (+ ord 'A')

conn :: Int -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
conn n = (wrap x (y - 1), wrap (x + 1) y, wrap x (y + 1), wrap (x - 1) y)
  where
    (y, x) = divMod n 3
    wrap x' y'
      | x' < 0 || 3 <= x' || y' < 0 || 3 <= y' = Nothing
      | otherwise = Just (x' + y' * 3)

direct :: (Int, Int, Int, Int)
direct = (2, 3, 0, 1)

panel :: Char -> (Int, Int, Int, Int)
panel '0' = (2, 3, 0, 1)
panel '1' = (1, 0, 3, 2)
panel '2' = (3, 2, 1, 0)

(.@.) :: (a, a, a, a) -> Int -> a
(n, _, _, _) .@. 0 = n
(_, n, _, _) .@. 1 = n
(_, _, n, _) .@. 2 = n
(_, _, _, n) .@. 3 = n
