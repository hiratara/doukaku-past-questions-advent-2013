module Doukaku.XYSort (solve) where
import Data.List (transpose, sort)
import Data.Char (ord)

table :: [[Int]]
table = [
  [4, 1, 4, 2, 1, 3]
  , [7, 3, 2, 0, 5, 0]
  , [2, 3, 6, 0, 6, 7]
  , [6, 4, 5, 7, 5, 1]
  , [3, 1, 6, 6, 2, 4]
  , [6, 0, 5, 5, 5, 1]
  ]

solve :: String -> String
solve = concatMap show . head . foldl (flip change) table

num :: Char -> Int
num c
  | c `elem` "ABCDEF" = ord c - ord 'A'
  | otherwise         = ord c - ord 'u'

change :: Char -> [[Int]] -> [[Int]]
change c
  | c `elem` "ABCDEF" = transpose . change' (num c) . transpose
  | otherwise         = change' (num c)

change' :: Int -> [[Int]] -> [[Int]]
change' n tb = map ((tb !!) . snd) . sort . zip base $ [0..]
  where
    base = transpose tb !! n
