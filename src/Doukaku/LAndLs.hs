module Doukaku.LAndLs (solve) where
import Data.Char (digitToInt)

type L = (Pt, Pt, Pt)
type Pt = (Int, Int)

parse :: String -> (L, L)
parse (x1:y1:'-':x2:y2:'-':x3:y3:',':x1':y1':'-':x2':y2':'-':x3':y3':_) =
  (((digitToInt x1, digitToInt y1), (digitToInt x2, digitToInt y2),
    (digitToInt x3, digitToInt y3)),
   ((digitToInt x1', digitToInt y1'), (digitToInt x2', digitToInt y2'),
    (digitToInt x3', digitToInt y3')))

solve :: String -> String
solve input = show . length $ [(x, y) | x <- [0 .. 9], y <- [0 .. 9]
                                       , contains l1 (x, y) && contains l2 (x, y)]
  where
    (l1, l2) = parse input

contains :: L -> Pt -> Bool
contains (p1, p2, p3) p = squareContains (p1, p2) p || squareContains (p1, p3) p

squareContains :: (Pt, Pt) -> Pt -> Bool
squareContains ((x1, y1), (x2, y2))  (x, y) =
  (x1 - x) * (x2 - x) <= 0 && (y1 - y) * (y2 - y) <= 0
