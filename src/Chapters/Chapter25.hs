{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapters.Chapter25 where

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure = Compose <$> (pure . pure)

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
    foldMap f (Compose fga) = (foldMap . foldMap) f fga


instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
    traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga


class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g
    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id
    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b
data Const a b = Const a
data Drei a b c = Drei a b c
data SuperDrei a b c = SuperDrei a b
data SemiDrei a b c = SemiDrei a
data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

instance Bifunctor Const where
    bimap f _ (Const a) = Const (f a)

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

instance Bifunctor Either where
    bimap _ g (Right a) = Right (g a)
    bimap f _ (Left c)  = Left (f c)
