{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | Entry point for tests
module Main where


import Test.Framework

import  {-@ HTF_TESTS @-}  Language.Haskell.HWide.CabalTest

-- | Test method
main :: IO()
main = htfMain htf_importedTests
