{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Utils where

import qualified Data.Text as T


import Data.IxSet.Typed
import Data.List (sortBy)
import Data.Ord
import Data.Char
import qualified Data.Set as Set
import Text.XML.Cursor
import qualified Data.Map as Map
import           Text.XML

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

lastInSet :: (Ord a, Ord b) => (a -> b) -> IxSet ixs a -> Maybe a
lastInSet f = safeLast . sortBy (comparing f) . toList

firstInSet :: (Ord a, Ord b) => (a -> b) -> IxSet ixs a -> Maybe a
firstInSet f = safeHead . sortBy (comparing f) . toList


jsonField :: Int -> String -> String
jsonField n = map toLower . drop n



ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs


update1 :: (Indexable ixs a)
         => a -> (IxSet ixs a -> IxSet ixs a) -> IxSet ixs a -> IxSet ixs a
update1 new restrict ixset = insert new $
                     maybe ixset (flip delete ixset) $
                     getOne $ restrict ixset

delete1 :: (Indexable ixs a)
         => (IxSet ixs a -> IxSet ixs a) -> IxSet ixs a -> IxSet ixs a
delete1 restrict ixset = maybe ixset (flip delete ixset) $
                       getOne $ restrict ixset

-- | Select only those element nodes not containing the given attribute key/value pair.
attributeIsNot :: Name -> T.Text -> Axis
attributeIsNot n v c =
    case node c of
        NodeElement (Element _ as _) -> if Just v /= Map.lookup n as then [c] else []
        _ -> []

splitCaseFold :: Char -> T.Text -> [T.Text]
splitCaseFold c = map T.toCaseFold . T.split (== c)

splitSearch :: (Indexable ixs a,IsIndexOf T.Text ixs) => Char -> T.Text -> IxSet ixs a -> IxSet ixs a
splitSearch c prf ix
    | T.null prf = ix
    | otherwise  = let
        cmpts = map T.toCaseFold $ T.split (==c) prf
        exact = init cmpts
        pref  = prefixInterval $ last cmpts
        in (foldr (flip (@=)) ix exact) @>=< pref

substringAfter :: T.Text -> T.Text -> T.Text
substringAfter needle haystack =
  let (prefix,match) = T.breakOn needle haystack
  in if T.null match
    then prefix
    else T.drop (T.length needle) match
