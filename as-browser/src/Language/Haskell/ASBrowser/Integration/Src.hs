{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Integration.Src where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LBS


import Text.HTML.DOM (parseLBS)
import Text.XML hiding (parseLBS)
import Text.XML.Cursor
import Network.HTTP.Conduit (simpleHttp)

-- import Language.Haskell.Exts.Annotated as HSE

import Language.Haskell.ASBrowser.Types as ASB


import Debug.Trace

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

parseHaddock :: LBS.ByteString -> [Decl]
parseHaddock bs = let
    d = parseLBS bs
    pr = Prologue [] Nothing []
    in parseHaddock' pr $ fromDocument d

parseHaddock' :: Prologue -> Cursor -> [Decl]
parseHaddock' pr c = c $.// tops >=> srcs
    where
        tops = element "div"
                       >=> attributeIs "class" "top"
        srcs s =  do
            src <- s $/ element "p"
                       >=> attributeIs "class" "src"
            ns <- src $/ names
            let k = src $/ kw
            let docs = s $/ doc
            -- let k'= trace (show docs) k
            let d = docToText docs
            [Decl (DeclName ns) (kw2Type k) "" d]
        names =  element "a"
                      >=> attributeIs "class" "def"
                      &// content
        kw = element "span"
                      >=> attributeIs "class" "keyword"
                      &// content
        doc = element "div"
                >=> attributeIs "class" "doc"
        kw2Type ("class":_)   = DeclClass
        kw2Type ["type"]    = DeclType
        kw2Type ["newtype"] = DeclNewType
        kw2Type ["data"]    = DeclData
        kw2Type _         = DeclFunction
        cToText [n] =  case node n of
            NodeElement el -> LT.toStrict $ renderText def (Document pr el [])
            _ -> ""
        cToText _ = ""
        docToText [n]     = let
                ps = n $/ element "p"
                ps2 = T.concat $ concatMap (\p -> p $// content) $ take 1 ps
                --ps2 = concatMap (\p-> p &// content) $ take 1 ps
                -- sh = T.concat ps2
                t = cToText [n]
                in Doc ps2 t
        docToText _      = Doc "" ""

