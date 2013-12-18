{-# LANGUAGE TupleSections #-}
module Doukaku.Gobang (solve) where
import Data.Char (intToDigit)

data Player = O | X deriving (Eq, Show)
type Gobang = [[(Char, Maybe Player)]]

solve :: String -> String
solve = go O X gobang
  where
    go _  _  _ [] = "Draw game."
    go p1 p2 g (h:hs) = case turn p1 h g of
      Nothing -> "Foul : " ++ toChar p2 : " won."
      Just g' | settled p1 g' -> toChar p1 : " won."
              | filled g'     -> "Draw game."
              | otherwise     -> go p2 p1 g' hs

toChar :: Player -> Char
toChar O = 'o'
toChar X = 'x'

gobang :: Gobang
gobang = map (line [0, 1, 2]) [1, 4, 7] ++
         map (line [0, 3, 6]) [1, 2, 3] ++
         map (line [0, 4, 8]) [1] ++
         map (line [0, 2, 4]) [3]
  where
    line :: [Int] -> Int -> [(Char, Maybe Player)]
    line xs x = map ((, Nothing) . intToDigit . (+ x)) $ xs

turn :: Player -> Char -> Gobang -> Maybe Gobang
turn p h = sequence . map (sequence . map (put p h))
  where
    put :: Player -> Char -> (Char, Maybe Player) -> Maybe (Char, Maybe Player)
    put p' h' x@(k, Nothing) | h' == k   = Just (k, Just p')
                             | otherwise = Just x
    put _ h' x@(k, _) | h' == k   = Nothing
                      | otherwise = Just x

settled :: Player -> Gobang -> Bool
settled p = any (all ((== Just p) . snd))

filled :: Gobang -> Bool
filled = all (all ((/= Nothing) . snd))
