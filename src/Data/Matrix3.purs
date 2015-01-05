-----------------------------------------------------------------------------
--
-- Module      :  Matrix3
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Matrix manipulation for WebGL
--
-----------------------------------------------------------------------------

module Data.Matrix3 where

import Data.TypeNat
import Data.Matrix
import Data.Maybe
import qualified Data.Vector3 as V3
import qualified Data.Vector as V

import Data.Array
import Prelude.Unsafe
import Math


type Mat3 = Mat Three Number

mat3 :: [Number] -> Mat3
mat3 = fromArray

normalFromMat4 :: Mat Four Number -> Maybe Mat3
normalFromMat4 (Mat [a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, a22, a23, a30, a31, a32, a33]) =
  let b00 = a00 * a11 - a01 * a10
      b01 = a00 * a12 - a02 * a10
      b02 = a00 * a13 - a03 * a10
      b03 = a01 * a12 - a02 * a11
      b04 = a01 * a13 - a03 * a11
      b05 = a02 * a13 - a03 * a12
      b06 = a20 * a31 - a21 * a30
      b07 = a20 * a32 - a22 * a30
      b08 = a20 * a33 - a23 * a30
      b09 = a21 * a32 - a22 * a31
      b10 = a21 * a33 - a23 * a31
      b11 = a22 * a33 - a23 * a32
        -- Calculate the determinant
      det_ = b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06

  in if (det_ == 0)
        then Nothing
        else let det = 1.0 / det_
              in Just $ Mat [
                      (a11 * b11 - a12 * b10 + a13 * b09) * det,
                      (a12 * b08 - a10 * b11 - a13 * b07) * det,
                      (a10 * b10 - a11 * b08 + a13 * b06) * det,

                      (a02 * b10 - a01 * b11 - a03 * b09) * det,
                      (a00 * b11 - a02 * b08 + a03 * b07) * det,
                      (a01 * b08 - a00 * b10 - a03 * b06) * det,

                      (a31 * b05 - a32 * b04 + a33 * b03) * det,
                      (a32 * b02 - a30 * b05 - a33 * b01) * det,
                      (a30 * b04 - a31 * b02 + a33 * b00) * det]
