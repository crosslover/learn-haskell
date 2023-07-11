{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad (ap, liftM3)
import Control.Monad.State
import GHC.Base (liftM)

data MovieReview = MovieReview
  { revTitle :: String,
    revUser :: String,
    revReview :: String
  }

lookup1 key alist = case lookup key alist of
  Just (Just s@(_ : _)) -> Just s
  _ -> Nothing

liftedReview alist =
  liftM3
    MovieReview
    (lookup1 "title" alist)
    (lookup1 "user" alist)
    (lookup1 "review" alist)

apReview alist =
  MovieReview
    `liftM` lookup1 "title" alist
    `ap` lookup1 "user" alist
    `ap` lookup1 "review" alist

data Context = Home | Mobile | Business
  deriving (Eq, Show)

type Phone = String

-- instance Applicative (Supply s) => Monad (Supply s) where
--   s >>= m = S (unwrapS s >>= unwrapS . m)
--   return = S . return