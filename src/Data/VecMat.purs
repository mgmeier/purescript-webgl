-----------------------------------------------------------------------------
--
-- Module      :  Data.VecMat
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Common definitions for Vectors and Matrixes
--
-----------------------------------------------------------------------------

module Data.VecMat where


-- * Type level numerals for phantom types
data Two
data Three
data Four

class TypeLevelNum a
instance typeLevelNumTwo :: TypeLevelNum Two
instance typeLevelNumThree :: TypeLevelNum Three
instance typeLevelNumFour :: TypeLevelNum Four

foreign import slice
  "function slice (s) {\
  \  return function (e) {\
  \    return function (l) {\
  \      return l.slice(s, e);\
  \    };\
  \  };\
  \}" :: forall a. Number -> Number -> [a] -> [a]
