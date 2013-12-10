module Doukaku.HoneycombTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Honeycomb as Honeycomb

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/honeycomb.tsv"
  , solve = Honeycomb.solve
  }
