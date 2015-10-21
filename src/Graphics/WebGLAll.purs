-----------------------------------------------------------------------------
--
-- Module      :  Graphics.WebGLAll
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

module Graphics.WebGLAll (
    module Graphics.WebGL
,   module Graphics.WebGLFramebuffer
,   module Graphics.WebGLTexture
,   module Control.Monad.Eff.WebGL
) where

import Graphics.WebGL
import Graphics.WebGLFramebuffer
import Graphics.WebGLTexture
import Control.Monad.Eff.WebGL
