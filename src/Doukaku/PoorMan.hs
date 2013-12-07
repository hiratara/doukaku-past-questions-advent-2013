module Doukaku.PoorMan (solve) where
import Data.Char (isDigit, digitToInt)
import Data.List

data Card = C Char Char | J deriving (Show)

instance Eq Card where
  J == J = True
  C _ n1 == C _ n2 = n1 == n2
  _ == _ = False

instance Ord Card where
  _ <= J = True
  C _ n1 <= C _ n2 = pow n1 <= pow n2
    where
      pow 'T' = 10
      pow 'J' = 11
      pow 'Q' = 12
      pow 'K' = 13
      pow 'A' = 14
      pow '2' = 15
      pow n | isDigit n = digitToInt n
      pow x = error $ "Unknown card: " ++ x : []
  _ <= _ = False

showC :: Card -> String
showC J = "Jo"
showC (C s n) = s:n:[]

showCs :: [Card] -> String
showCs = intercalate "" . map showC

showCss :: [[Card]] -> String
showCss [] = "-"
showCss css = intercalate "," . map showCs $ css

solve :: String -> String
solve input = showCss . filter (isSuitable fs) . pickCards (length fs) $ hs
  where
    (field, _:hand) = break (== ',') input
    fs = parse field
    hs = parse hand

parse :: String -> [Card]
parse ('J':'o':xs) = J : parse xs
parse (s:n:xs) = C s n : parse xs
parse _ = []

powerOfCards :: [Card] -> Card
powerOfCards cards = if null notJ then J else head notJ
  where
    notJ = filter (/= J) cards

isSuitable :: [Card] -> [Card] -> Bool
isSuitable fs hs = issamecards && powerOfCards fs < powerOfCards hs
  where
    notJ = filter (/= J) hs
    issamecards = if null notJ then True
                               else all (issame . head $ notJ) (tail notJ)
    issame (C _ n1) (C _ n2) = n1 == n2

pickCards :: Int -> [Card] -> [[Card]]
pickCards 0 _ = [[]]
pickCards _ [] = []
pickCards n (c:cs) = fmap (c :) (pickCards (n - 1) cs) ++ pickCards n cs
