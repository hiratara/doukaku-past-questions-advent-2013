module Doukaku.CrossCircle (solve) where

solve :: String -> String
solve = show . (`div` 2) . sum . map (uncurry countPoints) . searchLines

countPoints :: Eq a => [a] -> [a] -> Int
countPoints [] _ = 0
countPoints (x:xs) ys = (+ countPoints xs ys) . length . filter (== x) $ ys

searchLines :: Eq a => [a] -> [([a], [a])]
searchLines = searchLines' []

searchLines' :: Eq a => [a] -> [a] -> [([a], [a])]
searchLines' _ [] = []
searchLines' ds (x:xs) = (searchLines'' [] x xs ds) ++ searchLines' (x:ds) xs

searchLines'' :: Eq a => [a] -> a -> [a] -> [a] -> [([a], [a])]
searchLines'' _ _ [] _ = []
searchLines'' ds t0 (t:ts) cp
  | t == t0   = (ds, ts ++ cp) : tails
  | otherwise = tails
  where
    tails = searchLines'' (t:ds) t0 ts cp
