-- | UI utility functions
module Language.Haskell.HWide.UI.UIUtils where

import Graphics.UI.Threepenny.Core
import Control.Monad (void)
import qualified Data.Map as DM

import Language.Haskell.HWide.Util
import Language.Haskell.HWide.Internal.UniqueQueue

-- | Set a widget visible or not
setVisible :: Widget w => w -> Bool -> UI ()
setVisible el vis = void $ element el # set style (if vis then  [("display","block")] else [("display","none")])      


-- | Handler for run
type RunHandler = (RunToLogInput,RunToLogResult) -> UI ()

-- | Handling of runs
data RunHandling = RunHandling
  {
    rhBehavior :: Behavior (DM.Map RunToLogInput RunHandler)
  , rhFire     :: Handler (DM.Map RunToLogInput RunHandler -> DM.Map RunToLogInput RunHandler)
  , rhQueue    :: UniqueQueue RunToLogInput
  }
  
-- | Create a new run handler
mkRunHandling :: UI RunHandling
mkRunHandling = do
  (e,fire) <- liftIO newEvent
  b <- accumB DM.empty e
  (evtRunToLog,fireRunToLog) <- liftIO newEvent
  q <- liftIO $ startRunToLogQueue fireRunToLog
  onEvent evtRunToLog (\(i,r) -> do
    cb <- currentValue b
    let mf = DM.lookup i cb
    case mf of
      Nothing -> return ()
      Just f  -> do
         liftIO $ fire (DM.delete i)
         f (i,r)
    )
  return $ RunHandling b fire q

-- | Schedule a run with maybe an handler
scheduleRun :: (MonadIO m) => RunHandling -> RunToLogInput -> Maybe RunHandler -> m ()
scheduleRun (RunHandling _ fire q) i (Just f) = liftIO $ do
  fire (DM.insert i f)
  pushUnique q i
scheduleRun (RunHandling _ _ q) i _ = liftIO $ pushUnique q i
