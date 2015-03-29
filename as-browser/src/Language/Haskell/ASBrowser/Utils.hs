{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Utils where

import qualified Data.Text as T

import Data.IxSet
import Data.List
import Data.Ord
import Data.Char

prefixInterval :: T.Text -> (T.Text,T.Text)
prefixInterval "" = ("","")
prefixInterval txt = 
  let
    (h,t)=T.splitAt (T.length txt - 1) txt
  in (txt,T.snoc h (toEnum $ fromEnum (T.head t) + 1))

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ last xs

lastInSet :: (Ord a, Ord b) => (a -> b) -> IxSet a -> Maybe a
lastInSet f = safeLast . sortBy (comparing f) . toList

firstInSet :: (Ord a, Ord b) => (a -> b) -> IxSet a -> Maybe a
firstInSet f = safeHead . sortBy (comparing f) . toList


jsonField :: Int -> String -> String
jsonField n = map toLower . drop n