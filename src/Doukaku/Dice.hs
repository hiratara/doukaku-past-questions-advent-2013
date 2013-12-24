module Doukaku.Dice (solve) where
import Data.Char (intToDigit, digitToInt)

solve :: String -> String
solve = map head . scanl (flip roll) "153"

roll :: Char -> String -> String
roll 'E' (x:y:z:_) = z:y:(rev x):[]
roll 'W' ds = let e = roll 'E' in e . e . e $ ds
roll 'S' ds = let n = roll 'N' in n . n . n $ ds
roll 'N' (x:y:z:_) = y:(rev x):z:[]

rev :: Char -> Char
rev = intToDigit . (7 -) . digitToInt
