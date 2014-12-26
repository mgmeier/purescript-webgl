-----------------------------------------------------------------------------
--
-- Module      :  Graphics.WebGLTexture
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Textures for the WebGL binding for purescript
--
-----------------------------------------------------------------------------

module Graphics.WebGLTexture where

import Control.Monad.Eff.WebGL
import Graphics.WebGL
import Graphics.WebGLRaw
import Graphics.CanvasM

import Control.Monad.Eff


data TargetType =     TEXTURE_2D
                    | TEXTURE_CUBE_MAP_POSITIVE_X
                    | TEXTURE_CUBE_MAP_NEGATIVE_X
                    | TEXTURE_CUBE_MAP_POSITIVE_Y
                    | TEXTURE_CUBE_MAP_NEGATIVE_Y
                    | TEXTURE_CUBE_MAP_POSITIVE_Z
                    | TEXTURE_CUBE_MAP_NEGATIVE_Z

targetTypeToConst :: TargetType -> GLenum
targetTypeToConst TEXTURE_2D = _TEXTURE_2D
targetTypeToConst TEXTURE_CUBE_MAP_POSITIVE_X = _TEXTURE_CUBE_MAP_POSITIVE_X
targetTypeToConst TEXTURE_CUBE_MAP_NEGATIVE_X = _TEXTURE_CUBE_MAP_NEGATIVE_X
targetTypeToConst TEXTURE_CUBE_MAP_POSITIVE_Y = _TEXTURE_CUBE_MAP_POSITIVE_Y
targetTypeToConst TEXTURE_CUBE_MAP_NEGATIVE_Y = _TEXTURE_CUBE_MAP_NEGATIVE_Y
targetTypeToConst TEXTURE_CUBE_MAP_POSITIVE_Z = _TEXTURE_CUBE_MAP_POSITIVE_Z
targetTypeToConst TEXTURE_CUBE_MAP_NEGATIVE_Z = _TEXTURE_CUBE_MAP_NEGATIVE_Z

data InternalFormat =
  IF_ALPHA
  | IF_LUMINANCE
  | IF_LUMINANCE_ALPHA
  | IF_RGB
  | IF_RGBA

internalFormatToConst :: InternalFormat -> GLenum
internalFormatToConst IF_ALPHA     = _ALPHA
internalFormatToConst IF_LUMINANCE = _LUMINANCE
internalFormatToConst IF_LUMINANCE_ALPHA = _LUMINANCE_ALPHA
internalFormatToConst IF_RGB       = _RGB
internalFormatToConst IF_RGBA      = _RGBA

data TextureType =
  UNSIGNED_BYTE
  | RGBA
  | FLOAT
  | UNSIGNED_SHORT_5_6_5
  | UNSIGNED_SHORT_4_4_4_4
  | UNSIGNED_SHORT_5_5_5_1

textureTypeToConst :: TextureType -> GLenum
textureTypeToConst UNSIGNED_BYTE = _UNSIGNED_BYTE
textureTypeToConst RGBA = _RGBA
textureTypeToConst FLOAT = _FLOAT
textureTypeToConst UNSIGNED_SHORT_5_6_5 = _UNSIGNED_SHORT_5_6_5
textureTypeToConst UNSIGNED_SHORT_4_4_4_4 = _UNSIGNED_SHORT_4_4_4_4
textureTypeToConst UNSIGNED_SHORT_5_5_5_1 = _UNSIGNED_SHORT_5_5_5_1

data SymbolicParameter =
    PACK_ALIGNMENT
  | UNPACK_ALIGNMENT
  | UNPACK_FLIP_Y_WEBGL
  | UNPACK_PREMULTIPLY_ALPHA_WEBGL
  | UNPACK_COLORSPACE_CONVERSION_WEBGL

symbolicParameterToConst :: SymbolicParameter -> GLenum
symbolicParameterToConst PACK_ALIGNMENT = _PACK_ALIGNMENT
symbolicParameterToConst UNPACK_ALIGNMENT = _UNPACK_ALIGNMENT
symbolicParameterToConst UNPACK_FLIP_Y_WEBGL = _UNPACK_FLIP_Y_WEBGL
symbolicParameterToConst UNPACK_PREMULTIPLY_ALPHA_WEBGL = _UNPACK_PREMULTIPLY_ALPHA_WEBGL
symbolicParameterToConst UNPACK_COLORSPACE_CONVERSION_WEBGL = _UNPACK_COLORSPACE_CONVERSION_WEBGL

data TexTarget =
  TTEXTURE_2D
  | TTEXTURE_CUBE_MAP

texTargetToConst :: TexTarget -> GLenum
texTargetToConst TTEXTURE_2D = _TEXTURE_2D
texTargetToConst TTEXTURE_CUBE_MAP = _TEXTURE_CUBE_MAP

data TexParName =
  TEXTURE_MIN_FILTER
  | TEXTURE_MAG_FILTER
  | TEXTURE_WRAP_S
  | TEXTURE_WRAP_T
  | TEXTURE_MAX_ANISOTROPY_EXT

texParNameToConst :: TexParName -> GLenum
texParNameToConst TEXTURE_MIN_FILTER = _TEXTURE_MIN_FILTER
texParNameToConst TEXTURE_MAG_FILTER = _TEXTURE_MAG_FILTER
texParNameToConst TEXTURE_WRAP_S = _TEXTURE_WRAP_S
texParNameToConst TEXTURE_WRAP_T = _TEXTURE_WRAP_T
-- texParNameToConst TEXTURE_MAX_ANISOTROPY_EXT = _TEXTURE_MAX_ANISOTROPY_EXT

textureFor :: forall a eff. String -> (WebGLTexture -> EffWebGL eff a) -> EffWebGL eff Unit
textureFor name continuation = do
  texture <- createTexture
  loadImage name \image -> do
    handleLoad texture image
    continuation texture

handleLoad :: forall eff. WebGLTexture -> Image -> EffWebGL eff Unit
handleLoad texture image = do
  bindTexture_ TEXTURE_2D texture
  pixelStorei_ UNPACK_FLIP_Y_WEBGL 1
  texImage2D__ TEXTURE_2D 0 IF_RGBA IF_RGBA UNSIGNED_BYTE image
  texParameteri_ TTEXTURE_2D TEXTURE_MAG_FILTER _NEAREST
  texParameteri_ TTEXTURE_2D TEXTURE_MIN_FILTER _NEAREST
--  bindTexture _TEXTURE_2D 0

texParameteri_ :: forall eff. TexTarget -> TexParName -> GLint -> EffWebGL eff Unit
texParameteri_ target pname param = texParameteri (texTargetToConst target) (texParNameToConst pname) param

pixelStorei_ :: forall eff. SymbolicParameter -> Number -> EffWebGL eff Unit
pixelStorei_ symbolicParameter num = pixelStorei (symbolicParameterToConst symbolicParameter) num

bindTexture_ :: forall eff. TargetType -> WebGLTexture -> EffWebGL eff Unit
bindTexture_ tt texture = bindTexture (targetTypeToConst tt) texture

texImage2D__ :: forall eff. TargetType -> GLint -> InternalFormat -> InternalFormat -> TextureType -> Image
                    -> EffWebGL eff Unit
texImage2D__ target level internalFormat format typ pixels =
  texImage2D_ (targetTypeToConst target) level (internalFormatToConst internalFormat)
    (internalFormatToConst format) (textureTypeToConst typ) pixels


foreign import loadImage
"""
    function loadImage (name)
     {return function(continuation)
       {return function()
        {var i = new Image();
         i.src = name;
         i.onload = continuation (i);
          };};};"""
      :: forall a eff. String ->
                     (Image -> EffWebGL eff a)
                     -> EffWebGL eff Unit

foreign import texImage2D_
  """function texImage2D_(target)
   {return function(level)
    {return function(internalformat)
        {return function(format)
         {return function(type)
          {return function(pixels)
           {return function()
            {window.gl.texImage2D(target,level,internalformat,format,type,pixels);};};};};};};};"""
    :: forall a eff. GLenum->
                   GLint->
                   GLenum->
                   GLenum->
                   GLenum->
                   Image
                   -> EffWebGL eff Unit
