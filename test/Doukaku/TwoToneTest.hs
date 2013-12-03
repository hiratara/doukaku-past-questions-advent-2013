module Doukaku.TwoToneTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.TwoTone as TwoTone

tests :: IO [Test]
tests = createTests $ DoukakuTest "test/Doukaku/twotone.tsv" TwoTone.solve
