import Control.Monad (forM, when)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get, put), StateT (runStateT))
import Control.Monad.Writer (WriterT (runWriterT), runWriter, MonadWriter (tell))
import qualified Data.Functor
import System.Directory
import System.FilePath

data AppConfig = AppConfig
  { cfgMaxDepth :: Int
  }
  deriving (Show)

data AppState = AppState
  { stDeepestReached :: Int
  }
  deriving (Show)

type App = ReaderT AppConfig (StateT AppState (WriterT [String] IO))

-- type App = StateT AppState (ReaderT AppConfig IO)

-- runApp :: App a -> Int -> IO (a, AppState)
-- runApp k maxDepth =
--   let config = AppConfig maxDepth
--       state = AppState 0
--    in runReaderT (runStateT k state) config

runApp :: App a -> Int -> IO ((a, AppState), [String])
runApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
   in runWriterT (runStateT (runReaderT k config) state)

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  tell [path]
  rest <- forM contents $ \name -> do
    let newPath = path </> name
    tell [newPath]
    isDir <- liftIO $ doesDirectoryExist newPath
    if isDir && curDepth < cfgMaxDepth cfg
      then do
        let newDepth = curDepth + 1
        st <- get
        when (stDeepestReached st < newDepth) $
          put st {stDeepestReached = newDepth}
        constrainedCount newDepth newPath
      else return []
  return $ (path, length contents) : concat rest