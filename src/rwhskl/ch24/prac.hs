import Control.Concurrent

newtype BoundedChan a = BoundedChan (MVar [a])

-- newBoundedChanfunction :: Int -> IO (BoundedChan a)