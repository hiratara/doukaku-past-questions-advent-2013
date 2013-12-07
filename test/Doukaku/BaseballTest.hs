module Doukaku.BaseballTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Baseball as Baseball

tests :: IO [Test]
tests = createTests $ newDoukakuTest {tsvPath = "test/Doukaku/baseball.tsv", solve = Baseball.solve}
