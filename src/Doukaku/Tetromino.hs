module Doukaku.Tetromino (solve) where
import Data.Char (ord)
import Data.List (sort)
import Data.List.Split (splitOn)

type Pos = (Int, Int)

solve :: String -> String
solve = (: []) . check . normarize . parse

parse :: String -> [Pos]
parse = map (\ (x:y:_) -> (num x, num y)) . splitOn ","
  where
    num c = ord c - ord '0'

normarize :: [Pos] -> [Pos]
normarize poss = sort . (if xmax - xmin > ymax - ymin then trans else id) . move $ poss
  where
    minmax xs = (minimum xs, maximum xs)
    (xmin, xmax) = minmax . map fst $ poss
    (ymin, ymax) = minmax . map snd $ poss
    move ps = map (subtract xmin >< subtract ymin) $ ps
    trans ps = map (\ (x, y) -> (y, x)) $ ps
    (><) f g (x, y) = (f x, g y)

check :: [Pos] -> Char
check [(0, 0), (0, 1), (0, 2), (0, 3)] = 'I'
check [(0, 0), (0, 1), (1, 0), (1, 1)] = 'O'
check [(0, 0), (0, 1), (0, 2), (1, 0)] = 'L'
check [(0, 0), (0, 1), (0, 2), (1, 1)] = 'T'
check [(0, 0), (0, 1), (0, 2), (1, 2)] = 'L'
check [(0, 0), (1, 0), (1, 1), (1, 2)] = 'L'
check [(0, 1), (1, 0), (1, 1), (1, 2)] = 'T'
check [(0, 2), (1, 0), (1, 1), (1, 2)] = 'L'
check [(0, 0), (0, 1), (1, 1), (1, 2)] = 'S'
check [(0, 1), (0, 2), (1, 0), (1, 1)] = 'S'
check _ = '-'
