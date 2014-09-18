{-# LANGUAGE ExistentialQuantification #-}
-- | A button toggling the view of another element
module Language.Haskell.HWide.UI.PopupPane where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Language.Haskell.HWide.UI.UIUtils (setVisible)
import Control.Monad (void)

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
popupSpan :: (Widget w) 
  => (String,String) -- ^ classes to use when action is open/close
  -> (String,String) -- ^ title to use when action is open/close
  -> w -- ^ widget to toggle
  -> Event () -- ^ force close event
  -> UI (PopupPane w)
popupSpan (open,close) (openT,closeT) w eClose = do
  popupButton <- UI.span #. open # set UI.title__ openT
  popupPane popupButton (Just ((open,close),(openT,closeT))) w eClose


-- | Build the element
popupPane :: (Widget w) 
  => Element
  -> Maybe ((String,String) ,(String,String) )-- ^ classes to use when action is open/close, title to use when action is open/close
  -> w -- ^ widget to toggle
  -> Event () -- ^ force close event
  -> UI (PopupPane w)
popupPane popupButton m w eClose = do
  let eClick = const not <$> UI.click popupButton
      eForceClose = const (const False) <$> eClose
      eAll = concatenate <$> unions [eClick,eForceClose]
  -- Behavior
  bOpened <- accumB False eAll
  onChanges bOpened (\shouldShow -> do
    setVisible w shouldShow
    case m of
      Just ((open,close),(openT,closeT)) -> void $ element popupButton #. (if shouldShow then close else open)  # set UI.title__ (if shouldShow then closeT else openT)
      _ -> return ()
    )
  return $ PopupPane popupButton w bOpened
