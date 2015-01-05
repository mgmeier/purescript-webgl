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

import Data.Vector
import Data.TypeNat
import Data.Array
import Prelude.Unsafe
import Math

type Vec2 = Vec Two

vec2 :: forall a. a -> a -> Vec2 a
vec2 x y = Vec [x,y]

vec2' :: forall a. [a] -> Vec2 a
vec2' array | length array == 2 = Vec array

i :: Vec2 Number
i = Vec [1,0]

j :: Vec2 Number
j = Vec [0,1]

getX :: forall a. Vec2 a -> a
getX (Vec v) = unsafeIndex v 0

getY :: forall a. Vec2 a -> a
getY (Vec v) = unsafeIndex v 1

setX :: forall a. a -> Vec2 a -> Vec2 a
setX n (Vec v) = Vec (insertAt 0 n v)

setY :: forall a. a -> Vec2 a -> Vec2 a
setY n (Vec v) = Vec (insertAt 1 n v)
