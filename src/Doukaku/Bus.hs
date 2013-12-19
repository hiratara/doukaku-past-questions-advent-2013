module Doukaku.Bus (solve) where
import Data.List (sort)
import Data.List.Split (splitOn)

roundup :: Int -> Int
roundup n = ((n + 9) `div` 10) * 10

solve :: String -> String
solve input = show $ adultsFee + childrenFee + infantsFee
  where
    (base', _:people') = break (== ':') input
    base = read base'
    people = splitOn "," people'
    adults = filter ((== 'A') . head) people
    children = filter ((== 'C') . head) people
    infants' = filter ((== 'I') . head) people
    infants = drop (length adults * 2) . map snd . reverse . sort .
              zip (map (flip rate 100) infants') $ infants'
    adultsFee = sum . map (roundup . flip rate base) $ adults
    childrenFee = sum . map (roundup . (`div` 2) . flip rate base) $ children
    infantsFee = sum . map (roundup . (`div` 2) . flip rate base) $ infants

rate :: String -> (Int -> Int)
rate (_:'n':_) = id
rate (_:'p':_) = const 0
rate (_:'w':_) = (`div` 2)
rate (_:x:_) = error (x : " found")
