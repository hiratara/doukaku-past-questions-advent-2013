module Doukaku.LCoverTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.LCover as LCover

tests :: IO [Test]
tests = createTests $ DoukakuTest "test/Doukaku/lcover.tsv" LCover.solve
