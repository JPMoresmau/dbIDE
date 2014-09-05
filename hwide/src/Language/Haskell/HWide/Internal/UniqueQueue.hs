{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
-- | A queue system that doesn't allow duplicates
module Language.Haskell.HWide.Internal.UniqueQueue
  ( UniqueQueue
  , mkUniqueQueue
  , pushUnique
  , takeUnique
  )
  where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Typeable

import qualified Data.Set as DS

-- | A unique queue combines a normal Chan with a Set to avoid duplicates
data UniqueQueue a = (Ord a) => UniqueQueue (MVar (DS.Set a)) (Chan a)
  deriving (Typeable)

-- | Create an empty queue
mkUniqueQueue :: (Ord a) => IO (UniqueQueue a) 
mkUniqueQueue = do
  mv <- newMVar DS.empty 
  ch <- newChan
  return $ UniqueQueue mv ch

-- | Push an item onto the queue. Nothing happens if the item is already in the queue
pushUnique  :: (Ord a) => UniqueQueue a -> a -> IO ()
pushUnique (UniqueQueue mv ch) a= do
  s <- takeMVar mv
  let (push,s') = if a `DS.notMember` s
              then (Just a,DS.insert a s)
              else (Nothing, s)
  putMVar mv s'
  case push of
    Just p -> writeChan ch p
    _      -> return ()

-- | Get the next item in the queue
takeUnique :: (Ord a) => UniqueQueue a -> IO a
takeUnique (UniqueQueue mv ch) = do
  res <- readChan ch
  modifyMVar_ mv (return . DS.delete res) 
  return res

--mkUniqueQueue :: (Ord a) => IO (UniqueQueue a) 
--mkUniqueQueue = do
--  mv <- newMVar $ UniqueQueueData DS.empty Seq.empty
--  return $ UniqueQueue mv
--  
--pushUnique  :: (Ord a) => UniqueQueue a -> a -> IO ()
--pushUnique (UniqueQueue mv) a= do
--  dat@(UniqueQueueData s l) <- takeMVar mv
--  let dat' = if a `DS.notMember` s
--              then dat{uqSet=DS.insert a s,uqList=a <| l }
--              else dat
--  putMVar mv dat'
--
--takeUnique :: (Ord a) => UniqueQueue a -> IO (Maybe a)
--takeUnique (UniqueQueue mv) = do
--  dat@(UniqueQueueData s l) <- takeMVar mv
--  let (res,dat') = case viewr l of
--                    l' :> a -> (Just a,dat{uqSet=DS.delete a s,uqList=l' })
--                    _          -> (Nothing,dat)
--  putMVar mv dat'
--  return res
