module Doukaku.Tortoise (solve) where
import Data.Char (isDigit, digitToInt, ord)
import Data.List (zip4, zip5)

data Op = L | R | S deriving (Show, Eq)

solve :: String -> String
solve = uniq . map (maybe '?' fst) . scanl go (Just ('A', 0)) . concatMap parse
  where
    go :: Maybe (Char, Int) -> Op -> Maybe (Char, Int)
    go = (. move) . (>>=)
    uniq (x:y:ys)
      | x == y = uniq (y:ys)
      | otherwise = x:uniq (y:ys)
    uniq xs = xs
parse :: Char -> [Op]
parse 'L' = [L]
parse 'R' = [R]
parse c
  | isDigit c = replicate (digitToInt c) S
  | otherwise = replicate (10 + ord c - ord 'a') S

board :: [(Char, (Maybe Char, Maybe Char, Maybe Char, Maybe Char))]
board = ('A', (Just 'B', Just 'L', Nothing, Nothing)) :
        map (\(c, x, y, z) -> (c, (Just x, Just y, Just z, Nothing)) )
            (take 9 $ zip4 ['B'..] ['C'..] ['M'..] ['A'..]) ++
        [('K', (Just 'V', Just 'J', Nothing, Nothing))] ++
        [('L', (Just 'A', Just 'M', Just 'W', Nothing))] ++
        map (\(c, w, x, y, z) -> (c, (Just w, Just x, Just y, Just z)) )
            (take 9 $ zip5 ['M'..] ['N'..] (tail wline) ['L'..] ['B'..]) ++
        [('V', (Just 'g', Just 'U', Just 'K', Nothing))] ++
        [('W', (Just 'L', Just 'X', Just 'h', Nothing))] ++
        map (\(c, w, x, y, z') -> (c, (Just w, Just x, Just y, z')) )
            (take 9 $ zip5 (tail wline) wline ['M'..] (drop 2 wline) (tail hline')) ++
        [('g', (Just '5', Just 'f', Just 'V', Nothing))] ++
        [('j', (Just 'm', Just 'i', Just 'Y', Nothing))] ++
        map (\(c, x, y, z) -> (c, (Just x, Just y, Just z, Nothing)) )
            (take 7 $ zip4 (tail jline) (drop 2 jline) (tail iline) jline) ++
        [('7', (Just 'e', Just '6', Just '4', Nothing))] ++
        [('i', (Just 'j', Just 'l', Just 'h', Just 'X'))] ++
        map (\(c, w, x, y, z) -> (c, (Just w, Just x, Just y, Just z)) )
            (take 7 $ zip5 (tail iline) (drop 2 iline) (tail hline) iline (tail jline)) ++
        [('6', (Just 'f', Just '5', Just '3', Just '7'))] ++
        [('h', (Just 'W', Just 'i', Just 'k', Nothing))] ++
        map (\(c, x, y, z) -> (c, (Just x, Just y, Just z, Nothing)) )
            (take 7 $ zip4 (tail hline) hline (tail iline) (drop 2 hline)) ++
        [('5', (Just '2', Just '6', Just 'g', Nothing))]
  where
    wline = "WXYZ" ++ ['a'..]
    jline = ['j','m'..'y'] ++ "147"
    iline = ['i','l'..'x'] ++ "036"
    hline = ['h','k'..'z'] ++ "25"
    hline' = (map Just "hij") ++ replicate 5 Nothing ++ (map Just "765")

move :: Op -> (Char, Int) -> Maybe (Char, Int)
move L (c, n) = Just (c, (n + 3) `mod` 4)
move R (c, n) = Just (c, (n + 1) `mod` 4)
move S (c, n) = do
    c' <- lookup c board >>= get n
    n'' <- get' (Just c) `fmap` lookup c' board
    return (c', (n'' + 2) `mod` 4)
  where
    get 0 (x, _, _, _) = x
    get 1 (_, x, _, _) = x
    get 2 (_, _, x, _) = x
    get 3 (_, _, _, x) = x
    get' x (y1, y2, y3, y4)
      | x == y1 = 0
      | x == y2 = 1
      | x == y3 = 2
      | x == y4 = 3
