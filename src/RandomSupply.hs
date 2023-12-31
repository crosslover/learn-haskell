import Control.Arrow (first)
import Supply
import System.Random hiding (next)

randomsIO :: Random a => IO [a]
randomsIO =
  getStdRandom $ \g ->
    let (a, b) = split g
     in (randoms a, b)

randomsIO_golfed :: Random a => IO [a]
randomsIO_golfed = getStdRandom (first randoms . split)