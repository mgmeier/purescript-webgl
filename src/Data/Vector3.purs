-----------------------------------------------------------------------------
--
-- Module      :  Vector3
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

import Data.Vector
import Data.TypeNat
import Data.Array
import Prelude.Unsafe
import Math

type Vec3 = Vec Three

vec3 :: forall a. a -> a -> a -> Vec3 a
vec3 x y z = Vec [x,y,z]

vec3' :: forall a. [a] -> Vec3 a
vec3' array | length array == 3 = Vec array

i :: Vec3 Number
i = Vec [1,0,0]
j :: Vec3 Number
j = Vec [0,1,0]
k :: Vec3 Number
k = Vec [0,0,1]

getX :: forall a. Vec3 a -> a
getX (Vec v) = unsafeIndex v 0

getY :: forall a. Vec3 a -> a
getY (Vec v) = unsafeIndex v 1

getZ :: forall a. Vec3 a -> a
getZ (Vec v) = unsafeIndex v 2

setX :: forall a. a -> Vec3 a -> Vec3 a
setX n (Vec v) = Vec (insertAt 0 n v)

setY :: forall a. a -> Vec3 a -> Vec3 a
setY n (Vec v) = Vec (insertAt 1 n v)

setZ :: forall a. a -> Vec3 a -> Vec3 a
setZ n (Vec v) = Vec (insertAt 2 n v)

-- | The cross product of a and b
cross :: forall a. (Num a) => Vec3 a -> Vec3 a -> Vec3 a
cross (Vec [x1,y1,z1]) (Vec [x2,y2,z2]) = Vec [y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2]
