{-# LANGUAGE LambdaCase #-}
module Data.Functor.Sift where

import Data.Functor.Filterable
import Data.Functor.Union
import Data.Functor.HList

class Sift i o
  where
  sift :: (Traversable f, Filterable f) => f (i a) -> o (f a)

instance Sift (Union '[]) (HList '[])
  where
  sift _ = HNil

instance (Applicative x, Sift (Union xs) (HList xs)) => Sift (Union (x ': xs)) (HList (x ': xs))
  where
  sift vs = sequenceA a ::: sift b
    where
    (a, b) = partition $ fmap (\case { L x -> Left x; R y -> Right y }) $ vs
