{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import SupplyClass

newtype Reader e a = R {runReader :: e -> a}

instance Monad (Reader e) where
  return a = R $ \_ -> a
  m >>= k = R $ \r -> runReader (k (runReader m r)) r

instance Functor (Reader e)

fmap f m = R $ \r -> f (runReader m r)

instance Applicative (Reader e)

pure = R . const

f <*> x = R $ \r -> runReader f r (runReader x r)

ask :: Reader e e
ask = R id

-- newtype MySupply e a = MySupply {runMySupply :: Reader e a}
--   deriving (Monad)

-- instance MonadSupply e (MySupply e) where
--   next = MySupply (Just `liftM` ask)