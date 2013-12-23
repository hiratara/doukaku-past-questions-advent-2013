module Doukaku.DiceTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Dice as Dice

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/dice.tsv"
  , solve = Dice.solve
  }
