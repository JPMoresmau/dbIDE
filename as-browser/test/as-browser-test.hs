module Main where
import Language.Haskell.ASBrowser.Operations.ModulesTest
import Language.Haskell.ASBrowser.Operations.PackagesTest
import Language.Haskell.ASBrowser.UtilsTest

import Test.Tasty



main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [utilsTests,packageTests,moduleTests]
