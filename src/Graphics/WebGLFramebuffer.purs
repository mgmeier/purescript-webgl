-----------------------------------------------------------------------------
--
-- Module      :  Graphics.WebGLFramebuffer
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  juergen.nicklisch@symbolian.net
-- Stability   :
-- Portability :
--
-- | Framebuffers for the WebGL binding for purescript
--
-----------------------------------------------------------------------------

module Graphics.WebGLFramebuffer
(
    WebGLBuf(..)
    , createFramebuffer
    , bindFramebuffer
    , unbindFramebuffer
    , WebGLRendBuf(..)
    , createRenderbuffer
    , bindRenderbuffer
    , unbindRenderbuffer
    , RenderbufferFormat(..)
    , renderbufferStorage
    , AttachementPoint(..)
    , framebufferRenderbuffer

    , readPixels

)where

import Control.Monad.Eff.WebGL
import Graphics.WebGL
import Graphics.WebGLRaw
import Control.Monad.Eff
import Data.ArrayBuffer.Types

newtype WebGLBuf = WebGLBuf WebGLFramebuffer

newtype WebGLRendBuf = WebGLRendBuf WebGLRenderbuffer

data RenderbufferFormat = RGBA4 | RGB565 | RGB5_A1 | DEPTH_COMPONENT16

renderbufferFormatToConst :: RenderbufferFormat -> GLenum
renderbufferFormatToConst RGBA4 = _RGBA4
renderbufferFormatToConst RGB565 = _RGB565
renderbufferFormatToConst RGB5_A1 = _RGB5_A1
renderbufferFormatToConst DEPTH_COMPONENT16 = _DEPTH_COMPONENT16

data AttachementPoint = COLOR_ATTACHMENT0 | DEPTH_ATTACHMENT | STENCIL_ATTACHMENT | DEPTH_STENCIL_ATTACHMENT

attachementPointToConst :: AttachementPoint -> GLenum
attachementPointToConst COLOR_ATTACHMENT0 = _COLOR_ATTACHMENT0
attachementPointToConst DEPTH_ATTACHMENT  = _DEPTH_ATTACHMENT
attachementPointToConst STENCIL_ATTACHMENT = _STENCIL_ATTACHMENT
attachementPointToConst DEPTH_STENCIL_ATTACHMENT = _DEPTH_STENCIL_ATTACHMENT

createFramebuffer :: forall eff. EffWebGL eff WebGLBuf
createFramebuffer = do
  b <- createFramebuffer_
  return (WebGLBuf b)

bindFramebuffer :: forall eff. WebGLBuf -> EffWebGL eff Unit
bindFramebuffer (WebGLBuf buf) = bindFramebuffer_ _FRAMEBUFFER buf

unbindFramebuffer :: forall eff. EffWebGL eff Unit
unbindFramebuffer = unbindFramebuffer_ _FRAMEBUFFER

foreign import unbindFramebuffer_
  """function unbindFramebuffer_(target)
    {return function()
     {gl.bindFramebuffer(target,null);};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) Unit)

createRenderbuffer :: forall eff. EffWebGL eff WebGLRendBuf
createRenderbuffer = do
  b <- createRenderbuffer_
  return (WebGLRendBuf b)

bindRenderbuffer :: forall eff. WebGLRendBuf -> EffWebGL eff Unit
bindRenderbuffer (WebGLRendBuf buf) = bindRenderbuffer_ _RENDERBUFFER buf

unbindRenderbuffer :: forall eff. EffWebGL eff Unit
unbindRenderbuffer = unbindRenderbuffer_ _RENDERBUFFER

renderbufferStorage :: forall eff. RenderbufferFormat -> Number -> Number -> EffWebGL eff Unit
renderbufferStorage renderbufferFormat width height =
  renderbufferStorage_ _RENDERBUFFER (renderbufferFormatToConst renderbufferFormat) width height

framebufferRenderbuffer :: forall eff. AttachementPoint -> WebGLRendBuf ->  EffWebGL eff Unit
framebufferRenderbuffer attachementPoint (WebGLRendBuf buf) =
  framebufferRenderbuffer_ _FRAMEBUFFER (attachementPointToConst attachementPoint) _RENDERBUFFER buf

foreign import unbindRenderbuffer_
  """function unbindRenderbuffer_(target)
    {return function()
     {gl.bindRenderbuffer(target,null);};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) Unit)

foreign import readPixels
  """function readPixels(x)
   {return function(y)
    {return function(width)
     {return function(height)
      {return function(format)
       {return function(type)
        {return function(pixels)
         {return function()
          { var newPixels = pixels.slice();
            gl.readPixels(x,y,width,height,format,type,newPixels);
            return newPixels);};};};};};};};};
"""
    :: forall a eff. GLint->
                   GLint->
                   GLsizei->
                   GLsizei->
                   GLenum->
                   GLenum->
                   ArrayView a
                   -> (Eff (webgl :: WebGl | eff) (ArrayView a))
