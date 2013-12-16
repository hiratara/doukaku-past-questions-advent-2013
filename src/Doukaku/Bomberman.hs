{-# LANGUAGE Rank2Types #-}
module Doukaku.Bomberman where
import qualified Data.Array.IArray as IArray
import Control.Lens (review, preview)
import Control.Lens.Prism (Prism')
import Data.Array.Unboxed (UArray)
import Numeric.Lens (hex, binary)

type Board = UArray (Int, Int) Bool

solve :: String -> String
solve input = trans binary hex . (++ "00") . map char $ [0..width * height - 1]
  where
    (ws, _:bs) = break (== '/') input
    wall = parse ws
    bomb = parse bs
    char pos = let (y, x) = divMod pos width
               in if isFired wall bomb (x, y) then '1' else '0'

width, height :: Int
(width, height) = (6, 5)

trans :: Prism' String Int -> Prism' String Int -> String -> String
trans from to = tail . maybe "" (review to) . preview from . ('1' :)

parse :: String -> Board
parse input' = IArray.listArray ((0, 0), (height - 1, width - 1)) (map toBool input)
  where
    input = trans hex binary input'
    toBool '0' = False
    toBool _   = True

isFired :: Board -> Board -> (Int, Int) -> Bool
isFired wall bomb (x, y) = or . map (existBomb wall bomb (x, y)) $
                                             [(1, 0), (-1, 0), (0, 1), (0, -1)]

existBomb :: Board -> Board -> (Int, Int) -> (Int, Int) -> Bool
existBomb wall bomb (x, y) (vx, vy)
  | x < 0 || width <= x || y < 0 || height <= y = False
  | bomb IArray.! (y, x) = True
  | wall IArray.! (y, x) = False
  | otherwise = existBomb wall bomb (x + vx, y + vy) (vx, vy)
