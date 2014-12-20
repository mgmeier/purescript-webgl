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

module Data.Vector4 where

import Data.Array
import Prelude.Unsafe
import Math

newtype Vec4 = Vec4 [Number]

vector4 :: Number -> Number -> Number -> Number -> Vec4
vector4 x y z u = Vec4 [x,y,z,u]

vector4' :: [Number] -> Vec4
vector4' array | length array == 4 = Vec4 array

i :: Vec4
i = Vec4 [1,0,0,0]
j :: Vec4
j = Vec4 [0,1,0,0]
k :: Vec4
k = Vec4 [0,0,1,0]
l :: Vec4
l = Vec4 [0,0,0,1]

getX :: Vec4 -> Number
getX (Vec4 v) = unsafeIndex v 0

getY :: Vec4 -> Number
getY (Vec4 v) = unsafeIndex v 1

getZ :: Vec4 -> Number
getZ (Vec4 v) = unsafeIndex v 2

getU :: Vec4 -> Number
getU (Vec4 v) = unsafeIndex v 3

setX :: Number -> Vec4 -> Vec4
setX n (Vec4 v) = Vec4 (insertAt 0 n v)

setY :: Number -> Vec4 -> Vec4
setY n (Vec4 v) = Vec4 (insertAt 1 n v)

setZ :: Number -> Vec4 -> Vec4
setZ n (Vec4 v) = Vec4 (insertAt 2 n v)

setU :: Number -> Vec4 -> Vec4
setU n (Vec4 v) = Vec4 (insertAt 3 n v)

add :: Vec4 -> Vec4 -> Vec4
add (Vec4 [x1,y1,z1,u1]) (Vec4 [x2,y2,z2,u2]) = Vec4 [x1+x2,y1+y2,z1+z2,u1+u2]

sub :: Vec4 -> Vec4 -> Vec4
sub (Vec4 [x1,y1,z1,u1]) (Vec4 [x2,y2,z2,u2]) = Vec4 [x1-x2,y1-y2,z1-z2,u1-u2]

vnegate :: Vec4 -> Vec4
vnegate (Vec4 [x1,y1,z1,u1]) = Vec4 [-x1,-y1,-z1,-u1]

-- | The normalized direction from a to b: (a - b) / |a - b|
direction :: Vec4 -> Vec4 -> Vec4
direction v1 v2 = normalize (sub v1 v2)

-- | The length of the given vector: |a|
vlengthSquared :: Vec4 -> Number
vlengthSquared (Vec4 [x1,y1,z1,u1]) = x1*x1 + y1*y1 + z1*z1 + u1*u1

-- | The length of the given vector: |a|
vlength :: Vec4 -> Number
vlength = sqrt <<< vlengthSquared

-- |A unit vector with the same direction as the given vector: a / |a|
normalize :: Vec4 -> Vec4
normalize v@(Vec4 [x1,y1,z1,u1]) =
  let im = 1.0 / vlength v
  in Vec4 [x1 * im, y1*im, z1*im, u1*im]

-- | The distance between two vectors.
distanceSquared :: Vec4 -> Vec4 -> Number
distanceSquared (Vec4 [x1,y1,z1,u1]) (Vec4 [x2,y2,z2,u2]) =
  let dx = x1-x2
      dy = y1-y2
      dz = z1-z2
      du = u1-u2
  in dx * dx + dy * dy + dz * dz + du * du

-- | The distance between two vectors.
distance :: Vec4 -> Vec4 -> Number
distance v1 v2 = sqrt (distanceSquared v1 v2)

-- | Multiply the vector by a scalar: s * v
scale :: Number -> Vec4 -> Vec4
scale s (Vec4 [x1,y1,z1,u1]) = Vec4 [x1*s,y1*s,z1*s,u1*s]

-- | The dot product of a and b
dot :: Vec4 -> Vec4 -> Number
dot (Vec4 [x1,y1,z1,u1]) (Vec4 [x2,y2,z2,u2]) = x1*x2 + y1*y2 + z1*z2 * u1*u2
