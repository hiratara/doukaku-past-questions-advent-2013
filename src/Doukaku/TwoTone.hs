{-# LANGUAGE Rank2Types #-}
module Doukaku.TwoTone  where
import Data.Bits.Lens (bitAt)
import Numeric.Lens (hex)
import Control.Lens (Lens', view, set, preview, review)

solve :: String -> String
solve input = (n' ++) . (':' :) . fill . pack . turnRight l n . unpack $ tile'
  where
    (n', _:tile') = break (== ':') input
    n = read n'
    l = length tile'
    fill x = replicate (l - length x) '0' ++ x

unpack :: String -> Integer
unpack = maybe 0 id . preview hex

pack :: Integer -> String
pack = review hex

point :: Int -> Int -> Int -> Int -> Lens' Integer Bool
point l n x y = bitAt index
  where
    bitLength = l * 4
    index = (bitLength - 1) - (y * n + x)

turnRight :: Int -> Int -> Integer -> Integer
turnRight l n tile = foldr set' 0 points
  where
    points = [(x, y) | x <- [0 .. n - 1], y <- [0 .. n - 1]]
    set' (x, y) = set (point' x y) (view (point' y (n - 1 - x)) tile)
--    set' (x, y) = set (point' x y) (view (point' x y) tile)
    point' :: Int -> Int -> Lens' Integer Bool
    point' = point l n
