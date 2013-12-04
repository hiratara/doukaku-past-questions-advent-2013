module Doukaku.UpDownButtonTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.UpDownButton as UpDownButton

tests :: IO [Test]
tests = createTests $ DoukakuTest "test/Doukaku/updownbutton.tsv" UpDownButton.solve
