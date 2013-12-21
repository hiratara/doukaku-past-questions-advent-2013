{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Doukaku.TestHelper (DoukakuTest(..), newDoukakuTest, createTests) where

import Control.Exception (SomeException, catch)
import Distribution.TestSuite
import Data.List.Split
import Control.DeepSeq (($!!))
# if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 706)
# else
import Prelude hiding (catch)
# endif

data DoukakuTest = DoukakuTest {
    tsvPath :: FilePath
  , solve   :: String -> String
  , eq      :: String -> String -> Bool
  }

newDoukakuTest :: DoukakuTest
newDoukakuTest = DoukakuTest {
  tsvPath = error "You must specify the tsvPath field"
  , solve = error "You must specify the solve field"
  , eq = (==)
  }

wrapIOErrorProgress :: SomeException -> IO Progress
wrapIOErrorProgress = return . Finished . Fail . show

wrapIOErrorTest :: SomeException -> Test
wrapIOErrorTest e = Test $ TestInstance {
  run = wrapIOErrorProgress e
  , name = "Abort by exception"
  , tags = []
  , options = []
  , setOption = const . const . Right . unwrap . wrapIOErrorTest $ e
  }
  where
    unwrap (Test inst) = inst

createTests :: DoukakuTest -> IO [Test]
createTests def = createTests' `catch` (return . (: []) . wrapIOErrorTest)
  where
    createTests' = map (wrap (solve def) (eq def)) `fmap` testCases (tsvPath def)

testCases :: FilePath -> IO [(Int, String, String)]
testCases p = do
  content <- readFile p
  return (map parseLine . lines $ content)
  where
    parseLine l = let (n:input:output:_) = splitOn "\t" l
                  in (read n, input, output)

wrap :: (String -> String) -> (String -> String -> Bool) -> (Int, String, String) -> Test
wrap impl cmp (n, input, output) = Test $ TestInstance {
  run = run' `catch` wrapIOErrorProgress
  , name = "Test " ++ show n
  , tags = []
  , options = []
  , setOption = const . const . Right . unwrap . wrap impl cmp $ (n, input, output)
  }
  where
    unwrap (Test inst) = inst
    run' = do
      let solved = impl input
      let result = if solved `cmp` output
                   then Pass
                   else Fail . (++ " /= " ++ output) $!! solved
      return . Finished $! result
