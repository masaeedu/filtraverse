{-# LANGUAGE LambdaCase, EmptyCase #-}
module Data.Functor.Union where

data Union fs a
  where
  L :: f a        -> Union (f ': fs) a
  R :: Union fs a -> Union (f ': fs) a

instance Show (Union '[] a)
  where
  show = \case {}

instance (Show (x a), Show (Union xs a)) => Show (Union (x ': xs) a)
  where
  show (L x) = show x
  show (R x) = show x

class Inject fs f
  where
  inj :: f a -> Union fs a

instance {-# OVERLAPPING #-} Inject (f ': fs) f
  where
  inj = L

instance Inject fs g => Inject (f ': fs) g
  where
  inj = R . inj
