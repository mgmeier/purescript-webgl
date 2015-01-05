-----------------------------------------------------------------------------
--
-- Module      :  Data.TypeNat
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
--
-- | Simple type level naturals for vector and matrix sizes
--
-----------------------------------------------------------------------------

module Data.TypeNat where

-- * TypeLevelNat as phantom types
data Zero
data Suc a

newtype One   = One (Suc Zero)
newtype Two   = Two (Suc (Suc Zero))
newtype Three = Three (Suc (Suc (Suc Zero)))
newtype Four  = Four (Suc (Suc (Suc (Suc Zero))))

class Sized a where
  sized :: a -> Number
