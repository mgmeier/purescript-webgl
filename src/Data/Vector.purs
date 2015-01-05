-----------------------------------------------------------------------------
--
-- Module      :  Matrix
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Binding to mjs library
--
-----------------------------------------------------------------------------

module Data.Vector where

import Data.Array
import Data.Monoid
import Data.Foldable
import Data.TypeNat
import Control.Apply
import Prelude.Unsafe
import Math

newtype Vec s a = Vec [a]

class (Sized v) <= Vector v where

instance sv1 :: Sized (Vec One a) where
  sized v = 1
instance sv2 :: Sized (Vec Two a) where
  sized v = 2
instance sv3 :: Sized (Vec Three a) where
  sized v = 3
instance sv4 :: Sized (Vec Four a) where
  sized v = 4

fromArray :: forall s a. (Vector (Vec s a)) => [a] -> Vec s a
fromArray l =
  let res = Vec l
  in case sized res of
        i | i == length l -> res

toArray :: forall s a. Vec s a -> [a]
toArray (Vec a) = a

instance eqVec :: (Eq a) => Eq (Vec s a) where
  (==) (Vec l) (Vec r) = l == r
  (/=) (Vec l) (Vec r) = l /= r

instance showVec :: (Show a) => Show (Vec s a) where
  show (Vec l) = "Vec " ++ show l

instance functorVec :: Functor (Vec s) where
  (<$>) f (Vec l) = Vec (f <$> l)

instance applyVec :: Apply (Vec s) where
  (<*>) (Vec f) (Vec a) = Vec (zipWith (\f' a' -> f' a') f a)

instance foldableVector :: Foldable (Vec s) where
  foldr f z (Vec xs) = foldr f z xs
  foldl f z (Vec xs) = foldl f z xs
  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs

add :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a
add = lift2 (+)

sub :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a
sub = lift2 (-)

mult :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a
mult = lift2 (*)

vnegate :: forall a s. (Num a) => Vec s a -> Vec s a
vnegate v = negate <$> v

-- | The normalized direction from a to b: (a - b) / |a - b|
direction :: forall s. Vec s Number -> Vec s Number -> Vec s Number
direction v1 v2 = normalize (sub v1 v2)

-- | The length of the given vector: |a|
vlengthSquared :: forall s. Vec s Number -> Number
vlengthSquared v = foldl (+) 0 ((\e -> e * e) <$> v)

-- | The length of the given vector: |a|
vlength :: forall s. Vec s Number -> Number
vlength = sqrt <<< vlengthSquared
  -- :: forall a b. f (a -> b) -> f a -> f b

-- |A unit vector with the same direction as the given vector: a / |a|
normalize :: forall s. Vec s Number -> Vec s Number
normalize v =
  let im = 1.0 / vlength v
  in (\e -> e * im) <$> v

-- | The distance between two vectors.
distanceSquared :: forall s. Vec s Number -> Vec s Number -> Number
distanceSquared v1 v2 = foldl (+) 0 ((\e -> e * e) <$> (sub v1 v2))

-- | The distance between two vectors.
distance :: forall s. Vec s Number -> Vec s Number -> Number
distance v1 v2 = sqrt (distanceSquared v1 v2)

-- | Multiply the vector by a scalar: s * v
scale :: forall a s. (Num a) => a -> Vec s a -> Vec s a
scale s v = (\e -> e * s) <$> v

-- | The dot product of a and b
dot :: forall s . Vec s Number -> Vec s Number -> Number
dot v1 v2 = foldl (+) 0 (mult v1 v2)
