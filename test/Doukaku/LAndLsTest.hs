module Doukaku.LAndLsTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.LAndLs as LAndLs

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/landls.tsv"
  , solve = LAndLs.solve
  }
