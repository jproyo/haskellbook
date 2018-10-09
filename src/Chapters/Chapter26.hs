{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}


module Chapters.Chapter26 where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT eT) = EitherT $ (fmap . fmap) f eT

instance Applicative m => Applicative (EitherT e m) where
    pure eT = EitherT $ (pure . pure) eT
    (EitherT fab) <*> (EitherT ema) = EitherT $ (<*>) <$> fab <*> ema

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