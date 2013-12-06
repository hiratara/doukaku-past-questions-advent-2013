module Doukaku.TetrominoTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Tetromino as Tetromino

tests :: IO [Test]
tests = createTests $ DoukakuTest "test/Doukaku/tetromino.tsv" Tetromino.solve
