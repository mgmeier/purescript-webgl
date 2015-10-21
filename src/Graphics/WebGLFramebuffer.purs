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
    , checkFramebufferStatus
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
    , framebufferTexture2D

    , readPixels

)where

import Prelude
import Control.Monad.Eff.WebGL
import Graphics.WebGL
import Graphics.WebGLRaw
import Graphics.WebGLTexture
import Control.Monad.Eff
import Data.ArrayBuffer.Types
import Data.TypedArray
import Data.Function

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

checkFramebufferStatus :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) GLenum
checkFramebufferStatus = runFn1 checkFramebufferStatus_

createFramebuffer :: forall eff. EffWebGL eff WebGLBuf
createFramebuffer = do
  b <- runFn0 createFramebuffer_
  return (WebGLBuf b)

bindFramebuffer :: forall eff. WebGLBuf -> EffWebGL eff Unit
bindFramebuffer (WebGLBuf buf) = runFn2 bindFramebuffer_ _FRAMEBUFFER buf

unbindFramebuffer :: forall eff. EffWebGL eff Unit
unbindFramebuffer = runFn1 unbindFramebuffer_ _FRAMEBUFFER

createRenderbuffer :: forall eff. EffWebGL eff WebGLRendBuf
createRenderbuffer = do
  b <- runFn0 createRenderbuffer_
  return (WebGLRendBuf b)

bindRenderbuffer :: forall eff. WebGLRendBuf -> EffWebGL eff Unit
bindRenderbuffer (WebGLRendBuf buf) = runFn2 bindRenderbuffer_ _RENDERBUFFER buf

unbindRenderbuffer :: forall eff. EffWebGL eff Unit
unbindRenderbuffer = runFn1 unbindRenderbuffer_ _RENDERBUFFER

renderbufferStorage :: forall eff. RenderbufferFormat -> Int -> Int -> EffWebGL eff Unit
renderbufferStorage renderbufferFormat width height =
  runFn4 renderbufferStorage_ _RENDERBUFFER (renderbufferFormatToConst renderbufferFormat) width height

framebufferRenderbuffer :: forall eff. AttachementPoint -> WebGLRendBuf ->  EffWebGL eff Unit
framebufferRenderbuffer attachementPoint (WebGLRendBuf buf) =
  runFn4 framebufferRenderbuffer_ _FRAMEBUFFER (attachementPointToConst attachementPoint) _RENDERBUFFER buf

framebufferTexture2D :: forall eff. AttachementPoint -> TargetType -> WebGLTex -> EffWebGL eff Unit
framebufferTexture2D attachementPoint targetType (WebGLTex texture) =
  runFn5 framebufferTexture2D_ _FRAMEBUFFER (attachementPointToConst attachementPoint) (targetTypeToConst targetType) texture 0

readPixels :: forall eff. GLint ->
               GLint ->
               GLsizei ->
               GLsizei ->
               Uint8Array -> Eff (webgl :: WebGl | eff) Uint8Array
readPixels x y width height uint8Array =
  let copiedArray = asUint8Array (asArray uint8Array)
  in do
    runFn7 readPixels__ x y width height _RGBA _UNSIGNED_BYTE copiedArray
    return copiedArray

foreign import unbindRenderbuffer_ :: forall eff. Fn1 GLenum (Eff (webgl :: WebGl | eff) Unit)

foreign import unbindFramebuffer_ :: forall eff. Fn1 GLenum (Eff (webgl :: WebGl | eff) Unit)

foreign import readPixels__ :: forall a eff. Fn7 GLint
                   GLint
                   GLsizei
                   GLsizei
                   GLenum
                   GLenum
                   (ArrayView a)
                   (Eff (webgl :: WebGl | eff) Unit)
