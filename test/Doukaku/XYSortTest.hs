module Doukaku.XYSortTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.XYSort as XYSort

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/xysort.tsv"
  , solve = XYSort.solve
  }
