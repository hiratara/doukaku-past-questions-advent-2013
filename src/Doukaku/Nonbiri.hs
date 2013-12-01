module Doukaku.Nonbiri (solve) where
import Data.Char (toUpper)

solve :: String -> String
solve input = foldl next (take n . repeat $ '-') ope
  where
    ope :: String
    (n', ':':ope) = break (== ':') input
    n = read n' :: Int

next :: String -> Char -> String
next current c
  | 'a' <= c && c <= 'z' = let c' = toUpper c
                           in map (\x -> if x == c' then '-' else x) current
  | otherwise = sitDown c current

sideMembers :: String -> [Int]
sideMembers xs = sideMembers' ('-':xs)
  where
    sideMembers' (x:'-':[])   = [count x]
    sideMembers' (_:_:[]) = [-1]
    sideMembers' (x:'-':z:zs)   = let members = sum . map count $ [x, z]
                                  in members : sideMembers' ('-':z:zs)
    sideMembers' (_:y:z:zs) = -1 : sideMembers' (y:z:zs)
    count '-' = 0
    count  _  = 1

sitDown :: Char -> String -> String
sitDown new current = either id undefined $ do
  toEither (sitDown' 0 current sides)
  toEither (sitDown' 1 current sides)
  toEither (sitDown' 2 current sides)
  return ()
  where
    sides = sideMembers current
    sitDown' :: Int -> String -> [Int] -> Maybe String
    sitDown' _ [] [] = Nothing
    sitDown' n (c:cs) (m:ms)
      | n == m = Just $ new:cs
      | otherwise = (c :) `fmap` (sitDown' n cs ms)

toEither :: Maybe a -> Either a ()
toEither = maybe (Right ()) Left
