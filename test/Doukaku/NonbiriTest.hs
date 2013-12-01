module Doukaku.NonbiriTest (tests) where

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
    run = (return . Finished) (let result = impl input
                               in if result == output
                                  then Pass
                                  else Fail (result ++ " /= " ++ output))
  , name = "Test " ++ show n
  , tags = []
  , options = []
  , setOption = const . const . Right . unwrap . wrap impl $ (n, input, output)
  }
  where
    unwrap (Test inst) = inst
