module Data.Functor.Kit where

import Data.Functor.Const
import Data.Functor.Identity

newtype I a = I a
  deriving (Show, Functor)
  deriving Applicative via Identity

newtype K v a = K v
  deriving (Show, Functor)
  deriving Applicative via (Const v)

data (f :*: g) a
  where
  (:*:) :: f a -> g a -> (f :*: g) a
  deriving Functor

instance (Show (f a), Show (g a)) => Show ((f :*: g) a)
  where
  show (x :*: y) = show x <> " :*: " <> show y

instance (Applicative f, Applicative g) => Applicative (f :*: g)
  where
  pure a = pure a :*: pure a
  (f :*: g) <*> (a :*: b) = (f <*> a) :*: (g <*> b)

data (f :.: g) a
  where
  C :: f (g a) -> (f :.: g) a
  deriving Functor

instance (Applicative f, Applicative g) => Applicative (f :.: g)
  where
  pure a = C $ pure $ pure $ a
  C f <*> C g = C $ (<*>) <$> f <*> g

