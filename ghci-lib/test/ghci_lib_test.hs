module Main where

import Test.Tasty
import Language.Haskell.Ghci.ParserTest (parserTests)
import Language.Haskell.Ghci.HighLevelTests (highLevelTests)
import Language.Haskell.Ghci.UtilsTest (utilsTests)

main::IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" 
  [ utilsTests
  , parserTests
  , highLevelTests
  ]