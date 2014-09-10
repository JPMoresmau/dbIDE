-- | UI utility functions
module Language.Haskell.HWide.UI.UIUtils where

import Graphics.UI.Threepenny.Core ((#), element, set, style, UI, Widget)
import Control.Monad (void)


-- | Set a widget visible or not
setVisible :: Widget w => w -> Bool -> UI ()
setVisible el vis = void $ element el # set style (if vis then  [("display","block")] else [("display","none")])      


-- | Handler for run
-- type RunHandler i o= (i,o) -> UI ()
--
---- | Handling of runs
--data RunHandling i o = RunHandling
--  {
--    rhBehavior :: Behavior (DM.Map i (Handler (i,o)))
--  , rhFire     :: Handler (DM.Map i (Handler (i,o)) -> DM.Map i (Handler (i,o)))
--  , rhQueue    :: UniqueQueue i
--  }
--  
---- | Create a new run handler
--mkRunHandling :: UI (RunHandling RunToLogInput RunToLogResult)
--mkRunHandling = do
--  (e,fire) <- liftIO newEvent
--  b <- accumB DM.empty e
--  (evtRunToLog,fireRunToLog) <- liftIO newEvent
--  q <- liftIO $ startRunToLogQueue fireRunToLog
--  onEvent evtRunToLog (\(i,r) -> do
--    cb <- currentValue b
--    let mf = DM.lookup i cb
--    case mf of
--      Nothing -> return ()
--      Just f  -> liftIO $ do
--         fire (DM.delete i)
--         f (i,r)
--    )
--  return $ RunHandling b fire q
--
---- | Schedule a run with maybe an handler
--scheduleRun :: (MonadIO m,Ord i) => RunHandling i o -> i -> Maybe (Handler (i,o)) -> m ()
--scheduleRun (RunHandling _ fire q) i (Just f) = liftIO $ do
--  fire (DM.insert i f)
--  pushUnique q i
--scheduleRun (RunHandling _ _ q) i _ = liftIO $ pushUnique q i
