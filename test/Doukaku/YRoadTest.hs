module Doukaku.YRoadTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.YRoad as YRoad

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/yroad.tsv"
  , solve = YRoad.solve
  }
