module Doukaku.RailsTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Rails as Rails

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/rails.tsv"
  , solve = Rails.solve
  }
