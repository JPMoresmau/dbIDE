{-# LANGUAGE ExistentialQuantification #-}
-- | A button toggling the view of another element
module Language.Haskell.HWide.UI.PopupPane where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Language.Haskell.HWide.UI.UIUtils (setVisible)

-- | Build the widget
data PopupPane w = (Widget w) => PopupPane
  {
    ppButton :: Element -- ^ The toggle button
  , ppPane   :: w -- ^ The toggled element
  , ppBOpened:: Behavior Bool -- ^ Are we opened?
  }

-- | Widget instance
instance Widget (PopupPane w) where
  getElement = ppButton

-- | Build the element
popupPane :: (Widget w) 
  => (String,String) -- ^ classes to use when action is open/close
  -> (String,String) -- ^ title to use when action is open/close
  -> w -- ^ widget to toggle
  -> Event () -- ^ force close event
  -> UI (PopupPane w)
popupPane (open,close) (openT,closeT) w eClose = do
  
  popupButton <- UI.span #. open # set UI.title__ openT
  let eClick = const not <$> UI.click popupButton
      eForceClose = const (const False) <$> eClose
      eAll = concatenate <$> unions [eClick,eForceClose]
  -- Behavior
  bOpened <- accumB False eAll
  onChanges bOpened (\shouldShow -> do
    setVisible w shouldShow
    element popupButton #. (if shouldShow then close else open)  # set UI.title__ (if shouldShow then closeT else openT))
  return $ PopupPane popupButton w bOpened
  
