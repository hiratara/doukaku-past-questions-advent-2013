module Doukaku.UpDownButton (solve) where
import Data.Set (Set, empty, singleton, member, union, elems, fromList, difference)

solve :: String -> String
solve = show . minope empty . singleton . read

minope :: Set Int -> Set Int -> Int
minope done new'
  |   1  `member` new' = 1
  | (-1) `member` new' = 1
  | otherwise = 1 + minope (done `union` new') (new `difference` done)
  where
    new = fromList $ do
      x <- elems new'
      (if x `mod` 2 == 0 then [x `div` 2] else []) ++ [x + 1, x - 1]
