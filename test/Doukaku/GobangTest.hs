module Doukaku.GobangTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Gobang as Gobang

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/gobang.tsv"
  , solve = Gobang.solve
  }
