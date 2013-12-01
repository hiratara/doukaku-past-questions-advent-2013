{-# LANGUAGE ScopedTypeVariables #-}
module Doukaku.NonbiriTest (tests) where

import Distribution.TestSuite
import Doukaku.TestHelper
import qualified Doukaku.Nonbiri as Nonbiri

tests :: IO [Test]
tests = createTests $ DoukakuTest "test/Doukaku/nonbiri.tsv" Nonbiri.solve
