{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The entry point of the library
-- A lot of this work is Copyright Neil Mitchell 2014. See <https://github.com/ndmitchell/ghcid>
module Language.Haskell.GhciLib 
 ( T.GhciExec
 , T.GhciError (..)
 , T.Severity (..)
 , T.Load (..)
 , startGhci
 , showModules
 , reload
 , stopGhci
 )
where

import System.IO
import System.Process
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Function
import Data.List

import Language.Haskell.Ghci.Parser
import Language.Haskell.Ghci.Types as T
import Language.Haskell.Ghci.Utils

-- | Start GHCi, returning a function to perform further operation, as well as the result of the initial loading
startGhci :: String -> Maybe FilePath -> IO (GhciExec, Either GhciError [Load])
startGhci cmd directory = do
    (Just inp, Just out, Just err, _) <-
        createProcess (shell cmd){std_in=CreatePipe, std_out=CreatePipe, std_err=CreatePipe, cwd = directory}
    hSetBuffering out LineBuffering
    hSetBuffering err LineBuffering
    hSetBuffering inp LineBuffering

    lock <- newMVar () -- ensure only one person talks to ghci at a time
    let prefix = "#~GHC-LIB-START~#"
    let finish = "#~GHC-LIB-FINISH~#"
    hPutStrLn inp $ ":set prompt " ++ prefix

    -- consume from a handle, produce an MVar with either Just and a message, or Nothing (stream closed)
    let consume h name = do
            result <- newEmptyMVar -- the end result
            buffer <- newMVar [] -- the things to go in result
            forkIO $ fix $ \rec -> do
                el <- try $ hGetLine h
                case el of
                    Left (_ :: SomeException) -> putMVar result Nothing
                    Right l -> do
                        -- putStrLn $ "%" ++ name ++ ": " ++ l
                        if finish `isInfixOf` l
                          then do
                            buf <- modifyMVar buffer $ \old -> return ([], reverse old)
                            putMVar result $ Just buf
                          else
                            modifyMVar_ buffer $ return . (dropPrefix prefix l:)
                        rec
            return result

    outs <- consume out "GHCOUT"
    errs <- consume err "GHCERR"

    let f s = withMVar lock $ const $ do
        -- putStrLn $ "%GHCINP: " ++ s
        hPutStrLn inp $ s ++ "\nputStrLn " ++ show finish ++ "\nerror " ++ show finish
        outC <- takeMVar outs
        errC <- takeMVar errs
        case liftM2 (++) outC errC of
            Nothing  -> return $ Left $ UnexpectedExit cmd s 
            Just msg -> return $ Right msg
    r <- fmap (fmap (nub . parseLoad)) $ f ""
    return (f,r)


-- | Show modules
showModules :: GhciExec -> IO (Either GhciError[(String,FilePath)])
showModules exec= fmap (fmap parseShowModules) $ exec ":show modules"

-- | reload modules
reload :: GhciExec -> IO (Either GhciError [Load])
reload exec= fmap (fmap (nub . parseLoad)) $ exec ":r"

-- | Stop GHCi
stopGhci :: GhciExec -> IO ()
stopGhci exec = void $ exec ":q"
