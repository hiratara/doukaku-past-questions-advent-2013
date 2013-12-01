{-# LANGUAGE ScopedTypeVariables #-}
module Doukaku.TestHelper (DoukakuTest (..), createTests) where

import Control.Exception (SomeException, catch)
import Distribution.TestSuite
import Data.List.Split

data DoukakuTest = DoukakuTest {
    tsvPath :: FilePath
  , solve   :: String -> String
  }

createTests :: DoukakuTest -> IO [Test]
createTests def = map (wrap . solve $ def) `fmap` testCases (tsvPath def)

testCases :: FilePath -> IO [(Int, String, String)]
testCases p = do
  content <- readFile p
  return (map parseLine . lines $ content)
  where
    parseLine l = let (n:input:output:_) = splitOn "\t" l
                  in (read n, input, output)

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