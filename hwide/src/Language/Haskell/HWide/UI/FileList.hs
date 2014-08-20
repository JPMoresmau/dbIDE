module Language.Haskell.HWide.UI.FileList where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.FilePath              (takeFileName)
import           Data.List                    (sortBy, nub, elemIndex)
import           Data.IORef                   (atomicModifyIORef', newIORef)

import           Control.Monad                (forM, void)
import           Data.Ord                     (comparing)

data FileList = FileList 
  { flList  :: Element 
  , flAdd   :: FilePath -> UI()
  , flClose :: FilePath -> UI()
  , eSelection :: Event FilePath
  }

instance Widget FileList where
  getElement = flList

fileList ::  UI FileList
fileList = do  
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
      void $ return sel # set UI.selection (elemIndex fp fs)
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
  
--  on UI.selectionValueChange sel $ \s -> do
--    onOpen s
  --let eSel1 = filterJust $ UI.selectionChange sel
  --let eSel = fmap (\_ -> get value sel) eSel1
    
    
  return $ FileList sel addF closeF (UI.selectionValueChange sel)
  