module Language.Haskell.HWide.FileList where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.FilePath              (takeFileName)
import           Data.List                    (sortBy, nub)
import           Data.IORef                   (atomicModifyIORef', newIORef)

import           Control.Monad                (forM, void)
import           Data.Ord                     (comparing)

data FileList = FileList 
  { flList  :: Element 
  , flAdd   :: (FilePath -> UI())
  , flClose :: (FilePath -> UI())
  }


fileList :: (FilePath -> UI()) -> UI FileList
fileList onOpen = do  
  ior <- liftIO $ newIORef []
  sel <- UI.select  #. "fileList"
  let 
    addF :: FilePath -> UI()
    addF fp = do
      --opt <- UI.option # set value fp # set text (takeFileName fp)
      fs<- liftIO $ atomicModifyIORef' ior (\cs->let
        ncs=nub $ sortBy (comparing takeFileName) (fp:cs)
        in (ncs,ncs)) 
      setFs fs
    closeF ::FilePath -> UI()
    closeF fp = do
      fs<- liftIO $ atomicModifyIORef' ior (\fs->let
        fs2=filter (fp /=) fs
        in (fs2,fs2))
      setFs fs
    setFs :: [FilePath] -> UI()
    setFs fs = do
      opts <- forM fs (\fp -> UI.option # set value fp # set text (takeFileName fp))
      void $ return sel # set children opts
  
  on (domEvent "livechange") sel $ \_ -> do
    s <- get value sel
    onOpen s
    
  return $ FileList sel addF closeF
  