-----------------------------------------------------------------------------
--
-- Module      :  Effect.WebGL
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | WebGL binding for purescript
--
-----------------------------------------------------------------------------

module Effect.WebGL where

import Effect (Effect)

foreign import runWebGl_ :: forall a. Effect a -> Effect a
