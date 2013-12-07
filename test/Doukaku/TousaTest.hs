module Doukaku.TousaTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Tousa as Tousa

tests :: IO [Test]
tests = createTests $ newDoukakuTest {tsvPath = "test/Doukaku/tousa.tsv", solve = Tousa.solve}
