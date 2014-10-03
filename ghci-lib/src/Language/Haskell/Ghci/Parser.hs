{-# LANGUAGE PatternGuards #-}
-- | Parses the output from GHCi
-- A lot of this work is Copyright Neil Mitchell 2014. See <https://github.com/ndmitchell/ghcid>
module Language.Haskell.Ghci.Parser 
  ( parseShowModules
  , parseLoad
  )
  where

import System.FilePath
import Data.Char
import Data.List

import Language.Haskell.Ghci.Types

-- | Parse messages from show modules command
parseShowModules :: [String] -> [(String, FilePath)]
parseShowModules xs =
    [ (takeWhile (not . isSpace) $ dropWhile isSpace a, takeWhile (/= ',') b)
    | x <- xs, (a,'(':' ':b) <- [break (== '(') x]]


-- | Parse messages given on reload
parseLoad :: [String] -> [Load]
parseLoad (('[':xs):rest) =
    map (uncurry Loading) (parseShowModules [drop 11 $ dropWhile (/= ']') xs]) ++
    parseLoad rest
parseLoad (x:xs)
    | not $ " " `isPrefixOf` x
    , (file,':':rest) <- break (== ':') x
    , takeExtension file `elem` [".hs",".lhs"]
    , (pos,rest2) <- span (\c -> c == ':' || isDigit c) rest
    , [p1,p2] <- map read $ words $ map (\c -> if c == ':' then ' ' else c) pos 
    , (msg,las) <- span (isPrefixOf " ") xs
    , rest3 <- dropWhile isSpace rest2
    , sev <- if "Warning:" `isPrefixOf` rest3 then Warning else Error
    = Message sev file (p1,p2) (x:msg) : parseLoad las
parseLoad (_:xs) = parseLoad xs
parseLoad [] = []