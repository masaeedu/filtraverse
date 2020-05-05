module Data.Functor.Filterable where

import Data.Bifunctor

class Functor f => Filterable f
  where
  partition :: f (Either a b) -> (f a, f b)

instance Filterable []
  where
  partition [] = ([], [])
  partition (Left x  : xs) = bimap (x :) id $ partition xs
  partition (Right x : xs) = bimap id (x :) $ partition xs
