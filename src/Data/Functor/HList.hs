module Data.Functor.HList where

data HList fs a
  where
  HNil :: HList '[] a
  (:::) :: f a -> HList fs a -> HList (f ': fs) a

instance Show (HList '[] a)
  where
  show HNil = "[]"

instance (Show (x a), Show (HList xs a)) => Show (HList (x ': xs) a)
  where
  show (x ::: xs) = show x <> " ::: " <> show xs
