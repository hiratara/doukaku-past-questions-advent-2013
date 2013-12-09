module Doukaku.Aquarium (solve) where
import Data.Char (digitToInt)
import qualified Data.Map as Map
import Control.Monad.State (State, get, modify, evalState)
import Control.Monad (forM)

data Cell = Empty | Wall | Water | Unknown | Checking deriving (Show, Eq)
type Aquarium = Map.Map Int (Map.Map Int Cell)

hight :: Int
hight = 10

solve :: String -> String
solve input = show count
  where
    aquarium = parse input
    count = flip evalState aquarium $ do
      cells <- forM [(x, y) | y <- [0 .. hight - 1], x <- [0 .. Map.size aquarium - 1]]
                    (\(x, y) -> getCell x y)
      return . length . filter (== Water) $ cells

parse :: String -> Aquarium
parse input = Map.fromList . zip [0 .. ] . map (Map.fromList . line) $ input
  where
    line c = let h = digitToInt c
             in map (\n -> if n < h then (n, Wall)
                                    else (n, Unknown) ) [0 .. hight - 1]

getCell :: Int -> Int -> State Aquarium Cell
getCell x y = do
  aquarium <- get
  if x < 0 || Map.size aquarium <= x || y < 0 || hight <= y
    then return Empty
    else do
      let cell = aquarium Map.! x Map.! y
      if cell /= Unknown
        then (return cell)
        else do
          modify (setCell x y Checking)
          thisCell <- loopD False False False
          modify (setCell x y thisCell)
          return thisCell
  where
    loopL True True True = return Water
    loopL True  r d = loopR True r d
    loopL False r d = do
      c <- getCell (x - 1) y
      case c of
        Empty    -> return Empty
        _        -> loopR True r d
    loopR True True True = return Water
    loopR l True  d = loopD l True d
    loopR l False d = do
      c <- getCell (x + 1) y
      case c of
        Empty    -> return Empty
        _        -> loopD l True d
    loopD True True True = return Water
    loopD l r True  = loopL l r True
    loopD l r False = do
      c <- getCell x (y - 1)
      case c of
        Checking -> loopL l r False
        Empty    -> return Empty
        _        -> loopL l r True

setCell :: Int -> Int -> Cell -> Aquarium -> Aquarium
setCell x y cell aqua = Map.insert x newCol aqua
  where
    newCol = Map.insert y cell (aqua Map.! x)
