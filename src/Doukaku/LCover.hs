module Doukaku.LCover (solve) where
import Data.List.Split (splitOn)
import Data.Char (ord)
import Data.Maybe (isJust)

type Dots = [(Int, Int)]
type LCover = ((Int, Int), (Int, Int), Int, Int)

boardsize :: Int
boardsize = 10

solve :: String -> String
solve = show' . minimum' . map minArea . allDirections . parse
  where
    allDirections :: Dots -> [Dots]
    allDirections = flip (scanr ($)) $ replicate 3 turnLeft
    minimum' xs = let xs' = filter isJust xs
                  in if null xs' then Nothing else minimum xs'
    show' Nothing = "-"
    show' (Just x) = show x

parse :: String -> Dots
parse = map pair . splitOn ","
  where
    pair (x:y:_) = (digit x, digit y)
    digit = (subtract (ord '0')) . ord

turnLeft :: Dots -> Dots
turnLeft = map (\(x, y) -> (boardsize - 1 - y, x))

area :: LCover -> Int
area ((x1, x2), (y1, y2), w, h) = (x2 - x1 + 1) * (y2 - y1 + 1) - w * h

isCovered :: Dots -> LCover -> Bool
isCovered ds ((x1, x2), (y1, y2), w, h) = all inIt ds
  where
    inIt (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2
                  && (x > x1 + w - 1 || y > y1 + h - 1)

minArea :: Dots -> Maybe Int
minArea ds = minimum' . map area . filter (isCovered ds) $ ls
  where
    ls = [((x1, x2), (y1, y2), w, h) | x1 <- [0..x2], w <- [1..x2 - x1],
                                       y1 <- [0..y2], h <- [1..y2 - y1]]
    x2 = maximum . map fst $ ds
    y2 = maximum . map snd $ ds
    minimum' [] = Nothing
    minimum' xs = Just . minimum $ xs
