module Doukaku.EntropyTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Entropy as Entropy

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/entropy.tsv"
  , solve = Entropy.solve
  }
