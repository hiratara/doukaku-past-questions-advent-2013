module Doukaku.Poker (solve) where
import Data.List.Split (splitWhen)
import Data.List (sort, group)

solve :: String -> String
solve = decide . sort . map length . group . sort . parse
  where
    decide [1,4] = "4K"
    decide [2,3] = "FH"
    decide [1,1,3] = "3K"
    decide [1,2,2] = "2P"
    decide [1,1,1,2] = "1P"
    decide _ = "--"

parse :: String -> [String]
parse = tail . splitWhen (`elem` "SHDC")
