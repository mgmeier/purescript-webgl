-----------------------------------------------------------------------------
--
-- Module      :  Vector4
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

import Data.Vector
import Data.TypeNat
import Data.Array
import Prelude.Unsafe
import Math

type Vec4 = Vec Four

vec4 :: forall a. a -> a -> a -> a -> Vec4 a
vec4 x y z u = Vec [x,y,z,u]

vec4' :: forall a. [a] -> Vec4 a
vec4' array | length array == 4 = Vec array

i :: Vec4 Number
i = Vec [1,0,0,0]
j :: Vec4 Number
j = Vec [0,1,0,0]
k :: Vec4 Number
k = Vec [0,0,1,0]
l :: Vec4 Number
l = Vec [0,0,0,1]

getX :: forall a. Vec4 a -> a
getX (Vec v) = unsafeIndex v 0

getY :: forall a. Vec4 a -> a
getY (Vec v) = unsafeIndex v 1

getZ :: forall a. Vec4 a -> a
getZ (Vec v) = unsafeIndex v 2

getU :: forall a. Vec4 a -> a
getU (Vec v) = unsafeIndex v 3

setX :: forall a. a -> Vec4 a -> Vec4 a
setX n (Vec v) = Vec (insertAt 0 n v)

setY :: forall a. a -> Vec4 a -> Vec4 a
setY n (Vec v) = Vec (insertAt 1 n v)

setZ :: forall a. a -> Vec4 a -> Vec4 a
setZ n (Vec v) = Vec (insertAt 2 n v)

setU :: forall a. a -> Vec4 a -> Vec4 a
setU n (Vec v) = Vec (insertAt 3 n v)
