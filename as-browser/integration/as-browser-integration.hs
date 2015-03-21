module Main where
import Language.Haskell.ASBrowser.Integration.CabalTest


import Test.Tasty


main :: IO()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [cabalTests]
