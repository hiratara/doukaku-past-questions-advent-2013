module Doukaku.Honeycomb (solve) where

type Point = (Double, Double)
type Direction = (Double, Double)

solve :: String -> String
solve = map snd . scanl (\ (p, _) d -> moveOn p d) ((0, 0), 'A') . map direct

direct :: Char -> Direction
direct '0' = (0, 1)
direct '1' = (cos (pi / 6), sin (pi / 6))
direct '2' = (cos (pi / 6), - sin (pi / 6))
direct '3' = (0, - 1)
direct '4' = (- cos (pi / 6), - sin (pi / 6))
direct '5' = (- cos (pi / 6), sin (pi / 6))

(.>.) :: Direction -> Direction -> Direction
(x, y) .>. (x', y') = (x + x', y + y')

move :: Point -> Direction -> Point
move (x, y) (dx, dy) = (x + dx, y + dy)

moveOn :: Point -> Direction -> (Point, Char)
moveOn p d = let p' = move p d
             in case whereIs p' of
               Just c -> (p', c)
               Nothing -> (p, '!')

points :: [(Point, Char)]
points = zip points' alpha
  where
    alpha = ['A' .. 'Z'] ++ ['a' .. 'k']
    circle n = reset $ concatMap (replicate n . direct) "234501"
      where
        reset [] = direct '0' : []
        reset ds = init ds ++ (last ds .>. direct '0') : []
    directions = concatMap circle [0..]
    points' = scanl move (0, 0) directions

whereIs :: Point -> Maybe Char
whereIs pt = fmap snd . head' . filter (inIt pt . fst) $ points
  where
    head' [] = Nothing
    head' xs = Just . head $ xs

inIt :: Point -> Point -> Bool
inIt (x, y) (cx, cy) = (x - cx) ^ (2 :: Int) + (y - cy) ^ (2 :: Int) < 0.5 ^ (2 :: Int)
