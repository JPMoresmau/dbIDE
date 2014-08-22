{-# LANGUAGE ExistentialQuantification #-}

module Language.Haskell.HWide.UI.PopupPane where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.IORef
import Control.Monad (void)
import Control.Arrow ((&&&))

data PopupPane w = (Widget w) => PopupPane
  {
    ppButton :: Element
  , ppPane   :: w
  , ppOpen   :: UI ()
  , ppClose  :: UI ()
  }

instance Widget (PopupPane w) where
  getElement = ppButton

popupPane :: (Widget w) => (String,String) -> w -> UI (PopupPane w)
popupPane (open,close) w = do
  ior <- liftIO $ newIORef False
  popupButton <- UI.span #. open
  on UI.click popupButton (\_ -> do
    shouldShow <- liftIO $ atomicModifyIORef' ior (not &&& not)
    element w # set UI.style (if shouldShow then [("display","block")] else [("display","none")])
    element popupButton #. (if shouldShow then close else open))
  let openF  = liftIO (writeIORef ior True) >> void (do
        element w # set UI.style [("display","block")]
        element popupButton #. close
        )
      closeF = liftIO (writeIORef ior False) >> void (do
        element w # set UI.style [("display","none")]
        element popupButton #. open)
  return $ PopupPane popupButton w openF closeF
  
