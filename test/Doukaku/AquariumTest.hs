module Doukaku.AquariumTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Aquarium as Aquarium

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/aquarium.tsv"
  , solve = Aquarium.solve
  }
