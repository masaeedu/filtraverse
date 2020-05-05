{-# LANGUAGE LambdaCase, EmptyCase #-}
module Main where

import Data.Functor.Kit
import Data.Functor.Union
import Data.Functor.HList
import Data.Functor.Sift

data Decl a
  = TypeSig String a
  | Law String a a
  | FunModel String a
  | TypeModel String a
  | Import a
  | Other a
  deriving (Show, Functor)

type Law a       = (String, a, a)
type FunModel a  = (String, a)
type TypeModel a = (String, a)

type Cases a =
  [ K [Law a]
  , K [FunModel a]
  , K [TypeModel a]
  , K [Decl a]
  ]

-- In a sense the applicatives above represent the least
-- information-preserving way in which we can collect the
-- results (i.e. a fold is the stupidest traversal).

-- Given some actual structured applicative (perhaps you
-- have some structured way of combining laws), you can
-- have a more interesting functor list here, and @sift@
-- will faithfully reconstruct your filtered results in
-- those applicatives.

convert :: Decl a -> Union (Cases a) b
convert (Law s a b)     = inj $ K $ [(s, a, b)]
convert (FunModel s a)  = inj $ K $ [(s, a)]
convert (TypeModel s a) = inj $ K $ [(s, a)]
convert d               = inj $ K $ [d]

yourthing :: [Decl a] -> HList (Cases a) [b]
yourthing = sift . fmap convert

main :: IO ()
main = do
  print $ yourthing $ id @[Decl Int] $ []
  -- K [] ::: K [] ::: K [] ::: K [] ::: []
  print $ yourthing $ id @[Decl Int] $ [Law "foo" 1 1, FunModel "bar" 2, Other 5, Import 10, Law "baz" 3 3]
  -- K [("foo",1,1),("baz",3,3)] ::: K [("bar",2)] ::: K [] ::: K [Other 5,Import 10] ::: []
