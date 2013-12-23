module Doukaku.Poker2013 (solve) where
import Data.List (sort, group, tails)
import Data.Char (intToDigit)

solve :: String -> String
solve input
  | 'T' `elem` (map fst cards) && 'A' `elem` (map fst cards) &&
                               samesuit cards == 5 && ordered cards == 5 = "RF"
  | samesuit cards == 5 && ordered cards == 5 = "SF"
  | samesuit cards == 5 = "FL"
  | ordered cards == 5 = "ST"
  | any (\cs -> samesuit cs == 4 && ordered cs == 4) card4s = "4SF"
  | samesuit cards == 4 = "4F"
  | ordered cards == 4 = "4S"
  | otherwise = "-"
  where
    card4s = map (\c -> filter (/= c) cards) cards
    cards = parse input
    samesuit, ordered :: [(Char, Char)] -> Int
    samesuit = maximum . map length . group . sort . map snd
    ordered hs = maximum . map (contains (map fst hs)) . tails $ numberorder
    contains _ [] = 0
    contains hs (x:xs)
      | x `elem` hs = 1 + contains hs xs
      | otherwise = 0

parse :: String -> [(Char, Char)]
parse ('1':'0':s:cs) = ('T', s) : parse cs
parse (n:s:cs) = (n, s) : parse cs
parse _ = []

numberorder :: [Char]
numberorder = 'A' : map intToDigit [2 .. 9] ++ "TJQKA"
