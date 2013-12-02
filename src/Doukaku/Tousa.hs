module Doukaku.Tousa (solve) where

import Data.Char (isDigit, ord)
import Data.List (tails)

solve :: String -> String
solve = show . maximum . map maxSeqLen . tails . parse

parse :: String -> [Int]
parse = map parse'
  where
    parse' c
      | isDigit c = ord c - ord '0'
      | otherwise = ord c - ord 'a' + 10

maxSeqLen :: [Int] -> Int
maxSeqLen [] = 0
maxSeqLen (_:[]) = 1
maxSeqLen (x:y:ys) = maximum [2 + maxSeqLen' (y + n) ys, maxSeqLen (x:ys)]
  where
    n = y - x
    maxSeqLen' next xs = if null left then 0 else 1 + maxSeqLen' (next + n) (tail left)
      where (_, left) = break (== next) xs
