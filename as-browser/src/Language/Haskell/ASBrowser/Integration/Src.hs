{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.ASBrowser.Integration.Src where

import Data.Monoid

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LBS


import Text.HTML.DOM (parseLBS)
import Text.XML hiding (parseLBS)
import Text.XML.Cursor
import Network.HTTP.Conduit (simpleHttp)
import           Text.Blaze.Html                 (toHtml)
import           Text.Blaze.Html.Renderer.Text (renderHtml)

-- import Language.Haskell.Exts.Annotated as HSE

import Language.Haskell.ASBrowser.Types as ASB
import Language.Haskell.ASBrowser.Utils

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

parseModuleHaddock :: Module -> IO (Maybe (Module,[Decl]))
parseModuleHaddock modl = case uDocURL $ modURLs modl of
    Nothing -> return Nothing
    Just du -> (Just . parseHaddock modl) <$> simpleHttp (T.unpack $ unURLName du)


parseHaddock :: Module -> LBS.ByteString -> (Module,[Decl])
parseHaddock modl  = parseHaddock' modl . fromDocument . parseLBS

parseHaddock' :: Module -> Cursor -> (Module,[Decl])
parseHaddock' modl c =
    let ts = c $.// tops >=> srcs "p" "div" False
        cs = c $.// conss >=> srcs "td" "td" True
        ms = c $.// meths >=> srcs "p" "div" False
        ds = ts ++ (map toCons cs) ++ (map toMeth ms)
        mdocs = c $.// moduleDoc &/ (doc "div")
        --mdocs' = trace (show mdocs) mdocs
        d = docToText mdocs
    in (modl{modDoc=d},ds)
    where
        tops = element "div"
                       >=> attributeIs "class" "top"
        conss = element "div"
                       >=> attributeIs "class" "subs constructors"
        meths = element "div"
                       >=> attributeIs "class" "subs methods"
        moduleDoc = element "div"
                       >=> attributeIs "id" "description"
        srcs el docEl rec s =  do
            let f = if rec then ($//) else ($/)
            src <- s `f` (element el
                       >=> attributeIs "class" "src")
            ns <- src $/ namesAnch &// content
            url <- src $/ namesAnch &| anc
            let srcurl = concat $ srcUrl src
            let urls = URLs (addSrcUrl srcurl) (addDocAnchor url)
            let k = src $/ kw
            let docs = s $/ doc docEl
            --let ns'= trace (show srcurl) ns
            let d = docToText docs
            [Decl (DeclKey (DeclName ns) (modKey modl)) (kw2Type k) "" d urls]
        namesAnch =  element "a"
                      >=> attributeIs "class" "def"
                      -- &// content
        anc = attribute "name"
        kw = element "span"
                      >=> attributeIs "class" "keyword"
                      &// content
        doc el = element el
                >=> attributeIs "class" "doc"
        kw2Type ("class":_)   = DeclClass
        kw2Type ["type"]    = DeclType
        kw2Type ["newtype"] = DeclNewType
        kw2Type ["data"]    = DeclData
        kw2Type _         = DeclFunction
        cToText [n] =  case node n of
            NodeElement el -> LT.toStrict $ renderHtml $ toHtml el
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
        toCons d = d{dType=DeclConstructor}
        toMeth d = d{dType=DeclMethod}
        addDocAnchor [] = Nothing
        addDocAnchor (url:_) = (URL . (\t->t <> "#" <> url) . unURLName) <$> (uDocURL $ modURLs modl)
        srcUrl src = src $/ element "a"
                      >=> attributeIs "class" "link"
                      >=> check (\cr->T.concat (cr $// content) == "Source")
                      &| attribute "href"
        addSrcUrl [] = Nothing
        addSrcUrl (url:_) = (URL . (\t->(T.dropWhileEnd (/= '/') t) <> url) . unURLName) <$> (uDocURL $ modURLs modl)

