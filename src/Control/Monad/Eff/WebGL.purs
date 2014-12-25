-----------------------------------------------------------------------------
--
-- Module      :  Control.Monad.Eff.WebGL
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

module Control.Monad.Eff.WebGL where

import Control.Monad.Eff

foreign import data WebGl :: !

type EffWebGL eff a = Eff (webgl :: WebGl | eff) a

foreign import runWebGl_ """
  function runWebGl_(f) {
      return f;
  }""" :: forall a e. Eff (webgl :: WebGl | e) a -> Eff e a
