module Doukaku.Entropy (solve) where

solve :: String -> String
solve = decrypt . concatMap parse

parse :: Char -> String
parse '0' = "0000"
parse '1' = "1000"
parse '2' = "0100"
parse '3' = "1100"
parse '4' = "0010"
parse '5' = "1010"
parse '6' = "0110"
parse '7' = "1110"
parse '8' = "0001"
parse '9' = "1001"
parse 'a' = "0101"
parse 'b' = "1101"
parse 'c' = "0011"
parse 'd' = "1011"
parse 'e' = "0111"
parse 'f' = "1111"

decrypt :: String -> String
decrypt input = maybe "*invalid*" id (decrypt' input)
  where
  decrypt' :: String -> Maybe String
  decrypt' ('0':'0':'0':xs) = fmap ('t' :) $ decrypt' xs
  decrypt' ('0':'0':'1':'0':xs)  = fmap ('s' :) $ decrypt' xs
  decrypt' ('0':'0':'1':'1':xs)  = fmap ('n' :) $ decrypt' xs
  decrypt' ('0':'1':'0':'0':xs) = fmap ('i' :) $ decrypt' xs
  decrypt' ('0':'1':'0':'1':'0':xs) = fmap ('d' :) $ decrypt' xs
  decrypt' ('0':'1':'0':'1':'1':'0':'1':xs) = fmap ('c' :) $ decrypt' xs
  decrypt' ('0':'1':'0':'1':'1':'1':xs) = fmap ('l' :) $ decrypt' xs
  decrypt' ('0':'1':'1':'0':xs) = fmap ('o' :) $ decrypt' xs
  decrypt' ('0':'1':'1':'1':xs) = fmap ('a' :) $ decrypt' xs
  decrypt' ('1':'0':xs) = fmap ('e' :) $ decrypt' xs
  decrypt' ('1':'1':'0':'0':xs) = fmap ('r' :) $ decrypt' xs
  decrypt' ('1':'1':'0':'1':xs) = fmap ('h' :) $ decrypt' xs
  decrypt' ('1':'1':'1':xs) = Just $ ':' : show (length input - length xs)
  decrypt' _ = Nothing

