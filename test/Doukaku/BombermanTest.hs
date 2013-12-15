module Doukaku.BombermanTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Bomberman as Bomberman

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/bomberman.tsv"
  , solve = Bomberman.solve
  }
