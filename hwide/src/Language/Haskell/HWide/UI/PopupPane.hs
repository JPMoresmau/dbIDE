{-# LANGUAGE ExistentialQuantification #-}

module Language.Haskell.HWide.UI.PopupPane where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


data PopupPane w = (Widget w) => PopupPane
  {
    ppButton :: Element
  , ppPane   :: w
  , ppBOpened:: Behavior Bool
  }

instance Widget (PopupPane w) where
  getElement = ppButton

popupPane :: (Widget w) => (String,String) -> (String,String) -> w -> Event () -> UI (PopupPane w)
popupPane (open,close) (openT,closeT) w eClose = do
  
  popupButton <- UI.span #. open # set UI.title__ openT
  let eClick = const not <$> UI.click popupButton
      eForceClose = const (const False) <$> eClose
      eAll = concatenate <$> unions [eClick,eForceClose]
  bOpened <- accumB False eAll
  onChanges bOpened (\shouldShow -> do
    element w # set UI.style (if shouldShow then [("display","block")] else [("display","none")])
    element popupButton #. (if shouldShow then close else open)  # set UI.title__ (if shouldShow then closeT else openT))
  return $ PopupPane popupButton w bOpened
  
