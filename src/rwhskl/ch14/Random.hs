import Control.Monad.State (MonadState (..), State)
import System.Random (Random (random), StdGen)

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom =
  get >>= \gen ->
    let (val, gen') = random gen
     in put gen'
          >> return val

getRandom' :: Random a => RandomState a
getRandom' = do
  gen <- get
  let (val, gen') = random gen
  put gen'
  return val