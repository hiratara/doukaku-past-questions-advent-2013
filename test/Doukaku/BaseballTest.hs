module Doukaku.BaseballTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Baseball as Baseball

tests :: IO [Test]
tests = createTests $ DoukakuTest "test/Doukaku/baseball.tsv" Baseball.solve
