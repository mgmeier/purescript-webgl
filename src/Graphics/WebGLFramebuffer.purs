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
    , FrameBufferCode(..)
    , frameBufferCodeToConst
    , readPixels

)where

import Prelude (Unit, return, bind)
import Control.Monad.Eff.WebGL (WebGl, EffWebGL)
import Graphics.WebGLRaw (GLenum, GLsizei, GLint, WebGLRenderbuffer, WebGLFramebuffer, _UNSIGNED_BYTE, _RGBA, _FRAMEBUFFER, framebufferTexture2D_, _RENDERBUFFER, framebufferRenderbuffer_, renderbufferStorage_, bindRenderbuffer_, createRenderbuffer_, bindFramebuffer_, createFramebuffer_, checkFramebufferStatus_, _DEPTH_STENCIL_ATTACHMENT, _STENCIL_ATTACHMENT, _DEPTH_ATTACHMENT, _COLOR_ATTACHMENT0, _DEPTH_COMPONENT16, _RGB5_A1, _RGB565, _RGBA4)
import Graphics.WebGLTexture (TargetType, WebGLTex(WebGLTex), targetTypeToConst)
import Control.Monad.Eff (Eff)
import Data.ArrayBuffer.Types (ArrayView, Uint8Array)
import Data.TypedArray (asArray, asUint8Array)
import Data.Function (Fn7, Fn1, runFn7, runFn5, runFn4, runFn1, runFn2, runFn0)

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

data FrameBufferCode = FRAMEBUFFER_COMPLETE | FRAMEBUFFER_INCOMPLETE_ATTACHMENT | FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
    | FRAMEBUFFER_INCOMPLETE_DIMENSIONS | FRAMEBUFFER_UNSUPPORTED

frameBufferCodeToConst :: FrameBufferCode -> GLenum
frameBufferCodeToConst FRAMEBUFFER_COMPLETE = _COLOR_ATTACHMENT0
frameBufferCodeToConst FRAMEBUFFER_INCOMPLETE_ATTACHMENT  = _DEPTH_ATTACHMENT
frameBufferCodeToConst FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = _STENCIL_ATTACHMENT
frameBufferCodeToConst FRAMEBUFFER_INCOMPLETE_DIMENSIONS = _DEPTH_STENCIL_ATTACHMENT
frameBufferCodeToConst FRAMEBUFFER_UNSUPPORTED = _DEPTH_STENCIL_ATTACHMENT

_FRAMEBUFFER_COMPLETE :: Int
_FRAMEBUFFER_COMPLETE = 36053

_FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: Int
_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = 36054

_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: Int
_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = 36055

_FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: Int
_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = 36057

_FRAMEBUFFER_UNSUPPORTED :: Int
_FRAMEBUFFER_UNSUPPORTED = 36061

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
