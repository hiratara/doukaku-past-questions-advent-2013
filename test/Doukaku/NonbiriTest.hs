{-# LANGUAGE ScopedTypeVariables #-}
module Doukaku.NonbiriTest (tests) where

import Control.Exception (SomeException, catch)
import Distribution.TestSuite
import Data.List.Split
import qualified Doukaku.Nonbiri as Nonbiri

testCases :: IO [(Int, String, String)]
testCases = do
  content <- readFile "test/Doukaku/nonbiri.tsv"
  return (map parseLine . lines $ content)
  where
    parseLine l = let (n:input:output:_) = splitOn "\t" l
                  in (read n, input, output)

tests :: IO [Test]
tests = map (wrap Nonbiri.solve) `fmap` testCases

wrap :: (String -> String) -> (Int, String, String) -> Test
wrap impl (n, input, output) = Test $ TestInstance {
  run = run' `catch` (\(e :: SomeException) ->
                       return . Finished . Fail . show $ e)
  , name = "Test " ++ show n
  , tags = []
  , options = []
  , setOption = const . const . Right . unwrap . wrap impl $ (n, input, output)
  }
  where
    unwrap (Test inst) = inst
    run' = do
      let solved = impl input
      let result = if solved == output
                   then Pass
                   else Fail (solved ++ " /= " ++ output)
      return . Finished $! result
