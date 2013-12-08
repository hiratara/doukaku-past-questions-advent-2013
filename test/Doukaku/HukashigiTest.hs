module Doukaku.HukashigiTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Hukashigi as Hukashigi

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/hukashigi.tsv"
  , solve = Hukashigi.solve
  }
