{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- | Test Cabal handling
module Language.Haskell.HWide.CabalTest where


import Test.Framework
import Test.HUnit (Assertion)

import Language.Haskell.HWide.Cabal
import Language.Haskell.HWide.Util
import Language.Haskell.HWide.Notes
import Language.Haskell.HWide.TestUtil

-- | Test parsing build messages
test_parseBuildMessages :: Assertion
test_parseBuildMessages = do
  let err1="src/Main.hs:26:1:\n    Parse error: naked expression at top level\n    Perhaps you intended to use TemplateHaskell"
  (ss,_) <- getTestState
  let ns = parseBuildMessages ss (CachedFileInfo (Just "test.cabal") (Just "")) err1
  assertEqual 1 (length ns)
  assertEqual [BWNote {bwnStatus = BWError, bwnTitle = "Parse error: naked expression at top level\n    Perhaps you intended to use TemplateHaskell\n", bwnLocation = BWLocation {bwlSrc = "src/Main.hs", bwlLine = 26, bwlCol = 1, bwlEndLine = 26, bwlEndCol = 1}}] ns
