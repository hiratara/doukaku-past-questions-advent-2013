module Doukaku.NumbersTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Numbers as Numbers

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/numbers.tsv"
  , solve = Numbers.solve
  }
