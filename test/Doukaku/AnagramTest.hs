module Doukaku.AnagramTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Anagram as Anagram

tests :: IO [Test]
tests = createTests $ newDoukakuTest {
  tsvPath = "test/Doukaku/anagram.tsv"
  , solve = Anagram.solve
  }
