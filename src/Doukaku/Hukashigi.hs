module Doukaku.Hukashigi (solve) where
import Data.Char (ord)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

type Pt = (Int, Int)

areaSize :: Int
areaSize = 5

solve :: String -> String
solve input = show $ walk areaSize stop (0, 0) Set.empty
  where
    stop = parse input

parseChar :: Char -> Pt
parseChar = flipTupple . flip divMod areaSize . (subtract $ ord 'a') . ord
  where
    flipTupple (x, y) = (y, x)

parse :: String -> Set.Set (Pt, Pt)
parse input = Set.fromList . map parse' $ stops
  where stops = filter (not . null) . splitOn " " $ input
        parse' (x:y:_) = (parseChar x, parseChar y)

isStop :: Set.Set (Pt, Pt) -> Pt -> Pt -> Bool
isStop stop p1 p2 = Set.member (p1, p2) stop || Set.member (p2, p1) stop

walk :: Int -> Set.Set (Pt, Pt) -> Pt -> Set.Set Pt -> Int
walk size stop current@(x, y) done
  | current == (size - 1, size - 1) = 1
  | x < 0 || x >= size || y < 0 || y >= size = 0
  | otherwise = walk' (x, y - 1) + walk' (x, y + 1)
                + walk' (x - 1, y) + walk' (x + 1, y)
  where
    done' = Set.insert (x, y) done
    walk' pt' = if pt' `Set.member` done || isStop stop current pt'
                then 0
                else walk size stop pt' done'
