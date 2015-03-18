{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Utils where

import qualified Data.Text as T

prefixInterval :: T.Text -> (T.Text,T.Text)
prefixInterval "" = ("","")
prefixInterval txt = 
  let
    (h,t)=T.splitAt (T.length txt - 1) txt
  in (txt,T.snoc h (toEnum $ fromEnum (T.head t) + 1))
