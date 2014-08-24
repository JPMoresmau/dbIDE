-- | UI utility functions
module Language.Haskell.HWide.UI.UIUtils where

import Graphics.UI.Threepenny.Core
import Control.Monad (void)

-- | Set a widget visible or not
setVisible :: Widget w => w -> Bool -> UI ()
setVisible el vis = void $ element el # set style (if vis then  [("display","block")] else [("display","none")])      