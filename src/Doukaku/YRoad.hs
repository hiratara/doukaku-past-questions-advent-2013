{-# LANGUAGE TupleSections #-}
module Doukaku.YRoad (solve) where

solve :: String -> String
solve = map snd . scanl next' ('B', 'A')
  where
    next' (f, t) d = (t, next f t d)

route :: Char -> (Char, Char, Char)
route 'A' = ('D', 'C', 'B')
route 'B' = ('A', 'C', 'E')
route 'C' = ('A', 'F', 'B')
route 'D' = ('F', 'A', 'E')
route 'E' = ('B', 'F', 'D')
route 'F' = ('D', 'E', 'C')

next :: Char -> Char -> Char -> Char
next f t d = case d of
  'r' -> r
  'l' -> l
  _   -> f
  where
    (l, r) = case route t of
               (n1, n2, n3) | n1 == f -> (n2, n3)
                            | n2 == f -> (n3, n1)
                            | n3 == f -> (n1, n2)
