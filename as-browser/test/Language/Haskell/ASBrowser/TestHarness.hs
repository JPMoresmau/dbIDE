module Language.Haskell.ASBrowser.TestHarness where

import Data.Acid
import Data.Default

import Data.Acid.Memory

import Language.Haskell.ASBrowser.Database ()
import Language.Haskell.ASBrowser.Types

withTestAcid :: (AcidState Database -> IO b) -> IO b
withTestAcid f = openMemoryState (def::Database) >>= f