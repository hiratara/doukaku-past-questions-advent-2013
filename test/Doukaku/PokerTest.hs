module Doukaku.PokerTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Poker as Poker

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/poker.tsv"
  , solve = Poker.solve
  }
