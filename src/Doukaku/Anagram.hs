module Doukaku.Anagram (solve) where

solve :: String -> String
solve = show . maximum . map anagramLen' . allTails
  where
    anagramLen' ([], _) = 0
    anagramLen' (y:ys, xs) = max (anagramLen (y:ys) xs) (anagramLen ys xs + 1)

anagramLen :: String -> String -> Int
anagramLen [] _ = 0
anagramLen (x:xs) ys = max result (anagramLen xs ys)
  where
    (_, ys2') = break (== x) ys
    result = if null ys2' then 0 else 2 + anagramLen xs (tail ys2')

allTails :: String -> [(String, String)]
allTails = allTails' []
  where
    allTails' ys [] = (ys, []):[]
    allTails' ys (x:xs) = (ys, x:xs) : allTails' (x:ys) xs
