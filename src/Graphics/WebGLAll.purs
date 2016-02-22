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

import Graphics.WebGL (Bool, Buffer, ContextAttributes, Float, Mat2, Mat3, Mat4, Sampler2D, Vec2, Vec3, Vec4, WebGLContext, WebGLProg, Attribute(Attribute), BlendEquation(BLEND_EQUATION, BLEND_EQUATION_ALPHA, BLEND_EQUATION_RGB, FUNC_ADD, FUNC_REVERSE_SUBTRACT, FUNC_SUBTRACT), BlendingFactor(BLEND_COLOR, BLEND_DST_ALPHA, BLEND_DST_RGB, BLEND_SRC_ALPHA, BLEND_SRC_RGB, CONSTANT_ALPHA, CONSTANT_COLOR, DST_ALPHA, DST_COLOR, ONE, ONE_MINUS_CONSTANT_ALPHA, ONE_MINUS_CONSTANT_COLOR, ONE_MINUS_DST_ALPHA, ONE_MINUS_DST_COLOR, ONE_MINUS_SRC_ALPHA, ONE_MINUS_SRC_COLOR, SRC_ALPHA, SRC_ALPHA_SATURATE, SRC_COLOR, ZERO), BufferTarget(ARRAY_BUFFER, ELEMENT_ARRAY_BUFFER), Capacity(BLEND, CULL_FACE, DEPTH_TEST, POLYGON_OFFSET_FILL, SCISSOR_TEST), Func(ALWAYS, EQUAL, GEQUAL, GREATER, LEQUAL, LESS, NEVER, NOTEQUAL), Mask(COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT, STENCIL_BUFFER_BIT), Mode(LINES, LINE_LOOP, LINE_STRIP, POINTS, TRIANGLES, TRIANGLE_FAN, TRIANGLE_STRIP), Shaders(Shaders), Uniform(Uniform), bindBuf, bindBufAndSetVertexAttr, blendColor, blendEquation, blendEquationSeparate, blendFunc, blendFuncSeparate, clear, clearColor, clearDepth, clearStencil, colorMask, defContextAttributes, depthFunc, disable, disableVertexAttribArray, drawArr, drawElements, enable, enableVertexAttribArray, fillBuffer, getCanvasHeight, getCanvasWidth, isContextLost, isEnabled, makeBuffer, makeBufferDyn, makeBufferFloat, makeBufferFloatDyn, requestAnimationFrame, runWebGL, runWebGLAttr, setUniformBoolean, setUniformFloats, vertexPointer, viewport, withShaders)
import Graphics.WebGLFramebuffer (AttachementPoint(COLOR_ATTACHMENT0, DEPTH_ATTACHMENT, DEPTH_STENCIL_ATTACHMENT, STENCIL_ATTACHMENT), RenderbufferFormat(DEPTH_COMPONENT16, RGB565, RGB5_A1, RGBA4), WebGLBuf(WebGLBuf), WebGLRendBuf(WebGLRendBuf), bindFramebuffer, bindRenderbuffer, checkFramebufferStatus, createFramebuffer, createRenderbuffer, framebufferRenderbuffer, framebufferTexture2D, readPixels, renderbufferStorage, unbindFramebuffer, unbindRenderbuffer)
import Graphics.WebGLTexture (InternalFormat(IF_ALPHA, IF_LUMINANCE, IF_LUMINANCE_ALPHA, IF_RGB, IF_RGBA), SymbolicParameter(PACK_ALIGNMENT, UNPACK_ALIGNMENT, UNPACK_COLORSPACE_CONVERSION_WEBGL, UNPACK_FLIP_Y_WEBGL, UNPACK_PREMULTIPLY_ALPHA_WEBGL), TargetType(TEXTURE_2D, TEXTURE_CUBE_MAP_NEGATIVE_X, TEXTURE_CUBE_MAP_NEGATIVE_Y, TEXTURE_CUBE_MAP_NEGATIVE_Z, TEXTURE_CUBE_MAP_POSITIVE_X, TEXTURE_CUBE_MAP_POSITIVE_Y, TEXTURE_CUBE_MAP_POSITIVE_Z), TexFilterSpec(LINEAR, MIPMAP, NEAREST), TexParName(TEXTURE_MAG_FILTER, TEXTURE_MIN_FILTER, TEXTURE_WRAP_S, TEXTURE_WRAP_T), TexTarget(TTEXTURE_2D, TTEXTURE_CUBE_MAP), TextureType(FLOAT, RGBA, UNSIGNED_BYTE, UNSIGNED_SHORT_4_4_4_4, UNSIGNED_SHORT_5_5_5_1, UNSIGNED_SHORT_5_6_5), WebGLTex(WebGLTex), activeTexture, bindTexture, createTexture, handleLoad2D, newTexture, targetTypeToConst, texture2DFor, unbindTexture, withTexture2D)
import Control.Monad.Eff.WebGL (EffWebGL, WebGl, runWebGl_)
