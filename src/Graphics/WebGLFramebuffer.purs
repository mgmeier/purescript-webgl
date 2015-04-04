-----------------------------------------------------------------------------
--
-- Module      :  Graphics.WebGLFramebuffer
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Framebuffers for the WebGL binding for purescript
--
-----------------------------------------------------------------------------

module Graphics.WebGLFramebuffer
(
    WebGLBuf(..)
    , bindFramebuffer

)where

import Control.Monad.Eff.WebGL
import Graphics.WebGL
import Graphics.WebGLRaw
import Control.Monad.Eff

newtype WebGLBuf = WebGLBuf WebGLFramebuffer

bindFramebuffer :: forall eff. WebGLBuf -> EffWebGL eff Unit
bindFramebuffer (WebGLBuf buf) = bindFramebuffer_ _FRAMEBUFFER buf

bindCanvasbuffer :: forall eff. EffWebGL eff Unit
bindCanvasbuffer = bindCanvasbuffer_

foreign import bindCanvasbuffer_
  """function bindCanvasbuffer_
    {return function()
     {gl.bindFramebuffer(_FRAMEBUFFER,0);};};};"""
    :: forall eff. (Eff (webgl :: WebGl | eff) Unit)
