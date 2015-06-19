-- EitherT.hs
-- the EitherT monad transformer
-- follows closely the implementation of MaybeT
-- from Real World Haskell

module EitherT (EitherT(..), failEitherT) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Applicative (Applicative(..))

newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }

bindEitherT :: (Monad m) => EitherT a m b -> (b -> EitherT a m c) -> EitherT a m c
bindEitherT x f = EitherT $ do
  ret <- runEitherT x
  case ret of
    Left err -> return $ Left err
    Right y -> runEitherT (f y)

returnEitherT :: (Monad m) => b -> EitherT a m b
returnEitherT b = EitherT $ return (Right b)

instance (Monad m) => Monad (EitherT a m) where
  x >>= f = bindEitherT x f
  return  = returnEitherT

instance (Monad m) => Functor (EitherT a m) where
  fmap = liftM

instance (Monad m) => Applicative (EitherT a m) where
  pure  = return
  (<*>) = ap
  

liftEitherT :: (Monad m) => m b -> EitherT a m b
liftEitherT m = EitherT (Right `liftM` m)

instance MonadTrans (EitherT a) where
  lift = liftEitherT

failEitherT :: (Monad m) => a -> EitherT a m b
failEitherT a = EitherT $ return $ Left a
