module Doukaku.CrossCircleTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.CrossCircle as CrossCircle

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/crosscircle.tsv"
  , solve = CrossCircle.solve
  }
