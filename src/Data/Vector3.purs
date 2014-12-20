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

module Data.Vector3 where

import Data.Array
import Prelude.Unsafe
import Math

newtype Vec3 = Vec3 [Number]

vector3 :: Number -> Number -> Number -> Vec3
vector3 x y z = Vec3 [x,y,z]

vector3' :: [Number] -> Vec3
vector3' array | length array == 3 = Vec3 array

i :: Vec3
i = Vec3 [1,0,0]
j :: Vec3
j = Vec3 [0,1,0]
k :: Vec3
k = Vec3 [0,0,1]

getX :: Vec3 -> Number
getX (Vec3 v) = unsafeIndex v 0

getY :: Vec3 -> Number
getY (Vec3 v) = unsafeIndex v 1

getZ :: Vec3 -> Number
getZ (Vec3 v) = unsafeIndex v 2

setX :: Number -> Vec3 -> Vec3
setX n (Vec3 v) = Vec3 (insertAt 0 n v)

setY :: Number -> Vec3 -> Vec3
setY n (Vec3 v) = Vec3 (insertAt 1 n v)

setZ :: Number -> Vec3 -> Vec3
setZ n (Vec3 v) = Vec3 (insertAt 2 n v)

add :: Vec3 -> Vec3 -> Vec3
add (Vec3 [x1,y1,z1]) (Vec3 [x2,y2,z2]) = Vec3 [x1+x2,y1+y2,z1+z2]

sub :: Vec3 -> Vec3 -> Vec3
sub (Vec3 [x1,y1,z1]) (Vec3 [x2,y2,z2]) = Vec3 [x1-x2,y1-y2,z1-z2]

vnegate :: Vec3 -> Vec3
vnegate (Vec3 [x1,y1,z1]) = Vec3 [-x1,-y1,-z1]

-- | The normalized direction from a to b: (a - b) / |a - b|
direction :: Vec3 -> Vec3 -> Vec3
direction v1 v2 = normalize (sub v1 v2)

-- | The length of the given vector: |a|
vlengthSquared :: Vec3 -> Number
vlengthSquared (Vec3 [x1,y1,z1]) = x1*x1 + y1*y1 + z1*z1

-- | The length of the given vector: |a|
vlength :: Vec3 -> Number
vlength = sqrt <<< vlengthSquared

-- |A unit vector with the same direction as the given vector: a / |a|
normalize :: Vec3 -> Vec3
normalize v@(Vec3 [x1,y1,z1]) =
  let im = 1.0 / vlength v
  in Vec3 [x1 * im, y1*im, z1*im]

-- | The distance between two vectors.
distanceSquared :: Vec3 -> Vec3 -> Number
distanceSquared (Vec3 [x1,y1,z1]) (Vec3 [x2,y2,z2]) =
  let dx = x1-x2
      dy = y1-y2
      dz = z1-z2
  in dx * dx + dy * dy + dz * dz

-- | The distance between two vectors.
distance :: Vec3 -> Vec3 -> Number
distance v1 v2 = sqrt (distanceSquared v1 v2)

-- | Multiply the vector by a scalar: s * v
scale :: Number -> Vec3 -> Vec3
scale s (Vec3 [x1,y1,z1]) = Vec3 [x1*s,y1*s,z1*s]

-- | The dot product of a and b
dot :: Vec3 -> Vec3 -> Number
dot (Vec3 [x1,y1,z1]) (Vec3 [x2,y2,z2]) = x1*x2 + y1*y2 + z1*z2

-- | The cross product of a and b
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 [x1,y1,z1]) (Vec3 [x2,y2,z2]) = Vec3 [y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2]
