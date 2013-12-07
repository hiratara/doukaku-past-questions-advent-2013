module Doukaku.TetrominoTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Tetromino as Tetromino

tests :: IO [Test]
tests = createTests $ newDoukakuTest {tsvPath = "test/Doukaku/tetromino.tsv", solve = Tetromino.solve}
