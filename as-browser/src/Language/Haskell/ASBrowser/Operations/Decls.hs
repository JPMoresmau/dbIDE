{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Language.Haskell.ASBrowser.Operations.Decls where

import Control.Arrow ((***))
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.IxSet.Typed

import qualified Data.Text as T

import Language.Haskell.ASBrowser.Types
import Language.Haskell.ASBrowser.Utils

writeDecl :: Decl -> Update Database Decl
writeDecl de = do
  db@Database{..} <- get
  put $ db{dDecls=update1 de (declIx $ dKey de) dDecls}
  return de

deleteDecl :: DeclKey -> Update Database ()
deleteDecl key = do
  db@Database{..} <- get
  put $ db{dDecls=delete1 (declIx key) dDecls}

getDecl :: DeclKey -> Query Database (Maybe Decl)
getDecl key = do
  Database{..} <- ask
  return $ getOne $ ((dDecls @= (decName key)) &&& (dDecls @= (decModule key)))

declIx :: DeclKey -> IxSet DeclIxs Decl -> IxSet DeclIxs Decl
declIx key dDecls = ((dDecls @= (decName key)) &&& (dDecls @= (decModule key)))

listDecls :: ModuleKey -> Query Database IxDecl
listDecls key = do
  Database{..} <- ask
  return $ dDecls @= key

findDecls :: [ModuleKey] -> T.Text -> Query Database IxDecl
findDecls keys prf = do
  Database{..} <- ask
  let ix1 = case keys of
              [] -> dDecls
              _  -> dDecls @+ keys
  let ix2 = case prf of
              "" -> dDecls
              _  -> dDecls @>=< join (***) DeclName (prefixInterval prf)
  return $ ix1 &&& ix2
