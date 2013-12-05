module Doukaku.UpDownButton  where

import Numeric.Lens (decimal, binary)
import Control.Lens (preview, review)

toBool :: Char -> Bool
toBool '0' = False
toBool _   = True

toBinary :: String -> [Bool]
toBinary s = maybe [] id $ do
  binstr <- preview decimal s :: Maybe Int
  Just . map toBool . review binary $ binstr

count :: [Bool] -> Int
count = (\n -> n - 1) . countT 0 0
  where
    truesCost precost n = min (n * 2) (n + 3 - precost)
    countT precost acum [] = truesCost precost acum
    countT precost acum (True :xs) = countT precost (acum + 1) xs
    countT precost acum xs@(False:_) = truesCost precost acum + countF xs
    countF [] = 0
    countF xs@(True:_) = countT 1 0 xs
    countF (False:xs) = 1 + countF xs
solve :: String -> String
solve = show . count . toBinary
