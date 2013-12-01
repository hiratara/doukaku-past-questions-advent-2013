module Doukaku.Baseball (solve) where

import Data.List (intercalate)

solve :: String -> String
solve input = intercalate "," . map showBoard . tail $ boards
  where
    boards :: [Board]
    boards = scanl ((normarize .) . play) (0, 0, 0) input

type Board = (Int, Int, Int)

play :: Board -> Char -> Board
play (o, s, b) 's' = (o, s + 1, b)
play (o, s, b) 'b' = (o, s, b + 1)
play (o, s, b) 'f' | s <= 1    = (o, s + 1, b)
                   | otherwise = (o, s, b)
play (o, _, _) 'h' = (o, 0, 0)
play (o, _, _) 'p' = (o + 1, 0, 0)

normarize :: Board -> Board
normarize (o, s, b) | s >= 3 = normarize (o + 1, 0, b)
                    | b >= 4 = normarize (o, 0, 0)
                    | o >= 3 = normarize (0, 0, 0)
                    | otherwise = (o, s, b)

showBoard :: Board -> String
showBoard (o, s, b) = show o ++ show s ++ show b
