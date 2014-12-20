-----------------------------------------------------------------------------
--
-- Module      :  Vector2
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

module Data.Vector2 where

import Data.Array
import Prelude.Unsafe
import Math

newtype Vec2 = Vec2 [Number]

vector2 :: Number -> Number -> Vec2
vector2 x y = Vec2 [x,y]

vector2' :: [Number] -> Vec2
vector2' array | length array == 2 = Vec2 array

i :: Vec2
i = Vec2 [1,0]
j :: Vec2
j = Vec2 [0,1]

getX :: Vec2 -> Number
getX (Vec2 v) = unsafeIndex v 0

getY :: Vec2 -> Number
getY (Vec2 v) = unsafeIndex v 1

setX :: Number -> Vec2 -> Vec2
setX n (Vec2 v) = Vec2 (insertAt 0 n v)

setY :: Number -> Vec2 -> Vec2
setY n (Vec2 v) = Vec2 (insertAt 1 n v)

add :: Vec2 -> Vec2 -> Vec2
add (Vec2 [x1,y1]) (Vec2 [x2,y2]) = Vec2 [x1+x2,y1+y2]

sub :: Vec2 -> Vec2 -> Vec2
sub (Vec2 [x1,y1]) (Vec2 [x2,y2]) = Vec2 [x1-x2,y1-y2]

vnegate :: Vec2 -> Vec2
vnegate (Vec2 [x1,y1]) = Vec2 [-x1,-y1]

-- | The normalized direction from a to b: (a - b) / |a - b|
direction :: Vec2 -> Vec2 -> Vec2
direction v1 v2 = normalize (sub v1 v2)

-- | The length of the given vector: |a|
vlengthSquared :: Vec2 -> Number
vlengthSquared (Vec2 [x1,y1]) = x1*x1 + y1*y1

-- | The length of the given vector: |a|
vlength :: Vec2 -> Number
vlength = sqrt <<< vlengthSquared

-- |A unit vector with the same direction as the given vector: a / |a|
normalize :: Vec2 -> Vec2
normalize v@(Vec2 [x1,y1]) =
  let im = 1.0 / vlength v
  in Vec2 [x1 * im, y1*im]

-- | The distance between two vectors.
distanceSquared :: Vec2 -> Vec2 -> Number
distanceSquared (Vec2 [x1,y1]) (Vec2 [x2,y2]) =
  let dx = x1-x2
      dy = y1-y2
  in dx * dx + dy * dy

-- | The distance between two vectors.
distance :: Vec2 -> Vec2 -> Number
distance v1 v2 = sqrt (distanceSquared v1 v2)

-- | Multiply the vector by a scalar: s * v
scale :: Number -> Vec2 -> Vec2
scale s (Vec2 [x1,y1]) = Vec2 [x1*s,y1*s]

-- | The dot product of a and b
dot :: Vec2 -> Vec2 -> Number
dot (Vec2 [x1,y1]) (Vec2 [x2,y2]) = x1*x2 + y1*y2
