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
    , bindCanvasbuffer
    , createFramebuffer

)where

import Control.Monad.Eff.WebGL
import Graphics.WebGL
import Graphics.WebGLRaw
import Control.Monad.Eff

newtype WebGLBuf = WebGLBuf WebGLFramebuffer

createFramebuffer :: forall eff. EffWebGL eff WebGLBuf
createFramebuffer = do
  b <- createFramebuffer_
  return (WebGLBuf b)

bindFramebuffer :: forall eff. WebGLBuf -> EffWebGL eff Unit
bindFramebuffer (WebGLBuf buf) = bindFramebuffer_ _FRAMEBUFFER buf

bindCanvasbuffer :: forall eff. EffWebGL eff Unit
bindCanvasbuffer = bindCanvasbuffer_ _FRAMEBUFFER

foreign import bindCanvasbuffer_
  """function bindCanvasbuffer_(target)
    {return function()
     {gl.bindFramebuffer(target,null);};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) Unit)
