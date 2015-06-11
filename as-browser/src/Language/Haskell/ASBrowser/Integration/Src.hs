{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Integration.Src where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

import Control.Applicative

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
import Network.HTTP.Conduit (simpleHttp)
-- import Language.Haskell.Exts.Annotated as HSE

import Language.Haskell.ASBrowser.Types as ASB
import Data.Char
import Data.Maybe
import Data.String

type ModuleParseResult = Either [Marker] (Doc,[ASB.Decl])

--parseModule :: T.Text -> [T.Text] -> ModuleParseResult
--parseModule txt exts =
--  let
--    allExts = map (classifyExtension . T.unpack) exts
--    pm = defaultParseMode {extensions=allExts,ignoreLinePragmas=False,ignoreLanguagePragmas=False,fixities = Just baseFixities}
--    parseResult = parseFileContentsWithComments pm (preproc txt)
--  in case parseResult of
--    ParseFailed sloc err -> Left [Marker (T.pack err) MLError (Location (srcLine sloc) (srcColumn sloc))]
--    ParseOk res -> Right $ processModule $ associateHaddock res
--  where
--    preproc t = T.unpack $ T.unlines [if "#" `T.isPrefixOf` x then "" else x | x <- T.lines t]
--    processModule (HSE.Module (_,cmts) _ _ _ decls) = (processHaddock  cmts,catMaybes $ map processDecls decls)
--    processModule _ = error "unsupported module type"
--    processDecls (DataDecl (_,cmts) (DataType _) _ h cons _)= Decl <$> processDeclHead h <*> pure DeclData <*> pure "" <*> pure (processHaddock cmts)
--    processDecls _ = Nothing
--    processDeclHead (DHead _ n)=Just $ DeclName $ processName n
--    processDeclHead _ = Nothing
--    processName (Ident _ n)= T.pack n
--    processName (Symbol _ n)= T.pack n
--
--
--processHaddock :: [Comment] -> Doc
--processHaddock [] = Doc "" ""
--processHaddock (cmt:cmts)= Doc (T.pack $ unlines $ (processHaddock1 cmt) : map processHaddockr cmts) ""
--  where
--    trim = dropWhile isSpace
--    processHaddock1 (Comment _ _ txt) = trim $ case trim txt of
--      ('|':r) -> r
--      ('^':r) -> r
--      r       -> r
--    processHaddockr (Comment _ _ txt) = txt

getPreContentsURL :: T.Text -> IO T.Text
getPreContentsURL url = getPreContentsLBS <$> simpleHttp (T.unpack url)

getPreContentsLBS :: LBS.ByteString -> T.Text
getPreContentsLBS = getPreContents . fromDocument . parseLBS

getPreContents :: Cursor -> T.Text
getPreContents doc = T.concat $ doc $.// findNodes &| extractData
  where
    findNodes = element "pre" >=> descendant
    extractData = T.concat . content
