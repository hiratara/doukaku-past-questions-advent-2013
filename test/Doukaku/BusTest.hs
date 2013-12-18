module Doukaku.BusTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Bus as Bus

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/bus.tsv"
  , solve = Bus.solve
  }
