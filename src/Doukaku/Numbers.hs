module Doukaku.Numbers (solve) where
import Data.List (sort)
solve :: String -> String
solve input = let ns' = drop (read order - 1) . uniq . sort $ ds
              in if null ns' then "-" else head ns'
  where
    (order, _:is) = break (== ':') input
    is' = concatMap inflate . zip is $ [(0 :: Int)..]
      where
        inflate (d, n)
          | d `elem` "69" = [('6', n), ('9', n)]
          | otherwise = [(d, n)]
    ds = [[d1, d2, d3, d4] | (d1, n1) <- is', d1 /= '0',
                             (d2, n2) <- is', n2 /= n1,
                             (d3, n3) <- is', n3 `notElem` [n1, n2],
                             (d4, n4) <- is', n4 `notElem` [n1, n2, n3]]
    uniq (x:y:ys) = (if x == y then id else (x :)) . uniq $ (y:ys)
    uniq xs = xs
