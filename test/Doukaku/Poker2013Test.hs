module Doukaku.Poker2013Test (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Poker2013 as Poker2013

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/poker2013.tsv"
  , solve = Poker2013.solve
  }
