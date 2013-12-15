module Doukaku.Entropy (solve) where
import Numeric.Lens (hex, binary)
import Control.Lens (review, preview)

solve :: String -> String
solve = decrypt . concatMap parse

data BDD = Leaf Char | Done | Undefined | Node BDD BDD deriving (Eq, Show)

define :: [Char] -> Char -> BDD -> BDD
define [] c Undefined = if c == '\0' then Done else Leaf c
define ('1':bs) c bdd = case bdd of
  Undefined -> Node Undefined (define bs c Undefined)
  Node f t -> Node f (define bs c t)
define ('0':bs) c bdd = case bdd of
  Undefined -> Node (define bs c Undefined) Undefined
  Node f t -> Node (define bs c f) t
define _ c _ = error ("Bad definition of " ++ [c])

codebdd :: BDD
codebdd = define "000"     't'
        $ define "0010"    's'
        $ define "0011"    'n'
        $ define "0100"    'i'
        $ define "01010"   'd'
        $ define "0101101" 'c'
        $ define "010111"  'l'
        $ define "0110"    'o'
        $ define "0111"    'a'
        $ define "10"      'e'
        $ define "1100"    'r'
        $ define "1101"    'h'
        $ define "111"     '\0'
        $ Undefined

decrypt :: String -> String
decrypt is = maybe "*invalid*" answer $ decrypt' codebdd is
  where
    answer (n, s) = s ++ ':' : show n

decrypt' :: BDD -> String -> Maybe (Int, String)
decrypt' Done _ = Just (0, [])
decrypt' (Leaf c)  xs = do
  (n', cs) <- decrypt' codebdd xs
  return (n', c:cs)
decrypt' (Node f _) ('0':xs) = do
  (n', cs) <- decrypt' f xs
  return (n' + 1, cs)
decrypt' (Node _ t) ('1':xs) = do
  (n', cs) <- decrypt' t xs
  return (n' + 1, cs)
decrypt' _ _ = Nothing

parse :: Char -> String
parse c = reverse . tail . review binary $ n
  where n = maybe 0 id . preview hex $ '1':c:[] :: Int
