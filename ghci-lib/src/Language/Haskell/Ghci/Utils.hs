-- | Utility functions
-- A lot of this work is Copyright Neil Mitchell 2014. See <https://github.com/ndmitchell/ghcid>
module Language.Haskell.Ghci.Utils 
  ( dropPrefix
  )where

import Data.List (stripPrefix)

-- | Drop a prefix from a list, no matter how many times that prefix is present
dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix []  s = s
dropPrefix pre s = maybe s (dropPrefix pre) $ stripPrefix pre s