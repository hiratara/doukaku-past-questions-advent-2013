module Doukaku.TortoiseTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Tortoise as Tortoise

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/tortoise.tsv"
  , solve = Tortoise.solve
  }
