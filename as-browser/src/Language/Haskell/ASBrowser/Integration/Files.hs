module Language.Haskell.ASBrowser.Integration.Files where

import System.FilePath
import System.Directory

import Control.Applicative
import Control.Exception

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import Control.Monad
import Control.Concurrent
import Control.Concurrent.ParallelIO.Local

-- | Un-gzip and un-tar into a temporary folder, run the provided function, delete the directory
unTarGzipTemp :: FilePath -> (FilePath -> IO a) -> IO a
unTarGzipTemp res f = do
  tmp <- getTemporaryDirectory
  bracket (return $ tmp </> "as-browser")
          removeDirectoryRecursive
          (\tmpDir -> do
              unTarGzip res tmpDir
              f tmpDir)

unTarGzFileMap :: FilePath -> (FilePath -> BS.ByteString -> IO a) -> IO [a]
unTarGzFileMap res f = 
  walk =<< return . Tar.read . GZip.decompress =<< BS.readFile res
  where
    walk Tar.Done = return []
    walk (Tar.Fail _) = return []
    walk (Tar.Next e es) =
      case Tar.entryContent e of
        Tar.NormalFile bs _ -> do
          r <- f (Tar.entryPath e) bs
          (r:) <$> walk es
        _ -> walk es     
       
unTarGzFileParMap :: FilePath -> (FilePath -> BS.ByteString -> IO a) -> IO [a] 
unTarGzFileParMap res f = do
  cnts <- BS.readFile res
  let ungzip  = GZip.decompress cnts
      entries = Tar.read ungzip
      acts = Tar.foldEntries toAct [] (const []) entries
  n <- getNumCapabilities
  withPool n $ \pool -> parallelInterleaved pool acts
  where 
    toAct e ls =  
      case Tar.entryContent e of
        Tar.NormalFile bs _ -> f (Tar.entryPath e) bs : ls
        _ -> ls

-- | Un-gzip and un-tar a file into a folder.
unTarGzip :: FilePath -> FilePath -> IO ()
unTarGzip res folder = do
  cnts <- BS.readFile res
  let ungzip  = GZip.decompress cnts
      entries = Tar.read ungzip
  createDirectories entries
  Tar.unpack folder entries
  where createDirectories Tar.Done     = return ()
        createDirectories (Tar.Fail _) = return ()
        createDirectories (Tar.Next e es) =
            case Tar.entryContent e of
              Tar.NormalFile _ _ -> do let dir = folder </> takeDirectory (Tar.entryPath e)
                                       createDirectoryIfMissing True dir
                                       createDirectories es
              Tar.Directory      -> do let dir = folder </> Tar.entryPath e
                                       createDirectoryIfMissing True dir
                                       createDirectories es
              _                  -> createDirectories es

getSubDirs :: FilePath -> IO [FilePath]
getSubDirs folder =
  filterM isDir =<< getDirectoryContents folder
  where 
    isDir d = 
      if d `notElem` [".",".."]
          then doesDirectoryExist $ folder </> d
          else return False
