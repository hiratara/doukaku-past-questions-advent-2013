module Doukaku.PoorManTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.PoorMan as PoorMan
import Data.List (sort)
import Data.List.Split (splitOn)

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/poorman.tsv"
  , solve = PoorMan.solve
  , eq = eq'
  }
  where
    x `eq'` y = normarize x == normarize y
    normarize = sort . map normarize' . splitOn ","
    normarize' = concat . sort . twopieces
    twopieces :: String -> [String]
    twopieces [] = []
    twopieces ('-':ys) = ('-':[]) : twopieces ys
    twopieces (x:y:ys) = (x:y:[]) : twopieces ys
