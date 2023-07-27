-- file: ch24/NiceFork.hs
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module NiceFork
  ( ThreadManager,
    newManager,
    forkManaged,
    getStatus,
    waitFor,
    waitAll,
  )
where

import Control.Concurrent
import Control.Exception (Exception, SomeException, try)
import Control.Monad (join)
import qualified Data.Map as M

data ThreadStatus
  = Running
  | Finished -- 正常退出
  | Threw SomeException -- 被未捕获的异常终结
  deriving (Show)

newtype ThreadManager
  = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
  deriving (Eq)

-- | 创建一个新线程管理器
newManager :: IO ThreadManager
newManager = Mgr `fmap` newMVar M.empty

-- | 创建一个被管理的线程
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
  modifyMVar mgr $ \m -> do
    state <- newEmptyMVar
    tid <- forkIO $ do
      result <- try body
      putMVar state (either Threw (const Finished) result)
    return (M.insert tid state m, tid)

-- | 立即返回一个被管理线程的状态
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid =
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
      Nothing -> return (m, Nothing)
      Just st ->
        tryTakeMVar st >>= \mst -> case mst of
          Nothing -> return (m, Just Running)
          Just sth -> return (M.delete tid m, Just sth)

-- | 阻塞，直到某个特定的被管理线程终结
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid = do
  maybeDone <- modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, Nothing)
      (done, m') -> (m', done)
  case maybeDone of
    Nothing -> return Nothing
    Just st -> Just `fmap` takeMVar st

waitFor2 (Mgr mgr) tid =
  join . modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, return Nothing)
      (Just st, m') -> (m', Just `fmap` takeMVar st)

-- | 阻塞，直到所有被管理线程终结
waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
  where
    elems m = return (M.empty, M.elems m)