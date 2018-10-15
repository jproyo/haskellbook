{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Chapters.Chapter26 where

import           Control.Monad.Trans.Reader

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT eT) = EitherT $ (fmap . fmap) f eT

instance (Applicative m) => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))
    (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance Applicative m => Applicative (EitherT e m) where
    pure eT = EitherT $ (pure . pure) eT
    (EitherT fab) <*> (EitherT ema) = EitherT $ (<*>) <$> fab <*> ema

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (MaybeT ma) >>= f = MaybeT $ do
        v <- ma
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)

instance Monad m => Monad (EitherT e m) where
    return = pure
    (EitherT eT) >>= f = EitherT $ do
        v <- eT
        case v of
            Left l  -> return $ Left l
            Right r -> runEitherT (f r)

swapEither :: Either a b -> Either b a
swapEither (Right r) = Left r
swapEither (Left l)  = Right l

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT eT) = EitherT $ swapEither <$> eT

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fma gmb (EitherT eT) = eT >>= either fma gmb


newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT sT) = StateT $ \s -> ((,s) . f . fst) <$> sT s

instance (Monad m) => Applicative (StateT s m) where
    pure x = StateT $ \s -> pure (x, s)
    (StateT fab) <*> sma = StateT $ \s -> do
        (f, s') <- fab s
        runStateT (fmap f sma) s'



instance (Monad m) => Monad (StateT s m) where
    return = pure
    (StateT sma) >>= f = StateT $ \s -> do
        (ar, s') <- sma s
        runStateT (f ar) s'

class MonadTrans t where
    lift :: (Monad m) => m a -> t m a

instance MonadTrans MaybeT where
    lift = MaybeT . fmap Just

instance MonadTrans (EitherT e) where
    lift = EitherT . fmap Right


instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)

class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a

instance (MonadIO m) => MonadIO (EitherT e m) where
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO


rDec :: Num a => Reader a a
rDec = asks (\a -> a - 1)
