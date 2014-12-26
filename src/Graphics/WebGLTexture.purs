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

data TargetType =
                    TEXTURE_2D
                    | TEXTURE_CUBE_MAP_POSITIVE_X
                    | TEXTURE_CUBE_MAP_NEGATIVE_X
                    | TEXTURE_CUBE_MAP_POSITIVE_Y
                    | TEXTURE_CUBE_MAP_NEGATIVE_Y
                    | TEXTURE_CUBE_MAP_POSITIVE_Z
                    | TEXTURE_CUBE_MAP_NEGATIVE_Z

targetTypeToConst :: TargetType -> Number
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

internalFormatToConst :: InternalFormat -> Number
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

textureTypeToConst :: TextureType -> Number
textureTypeToConst UNSIGNED_BYTE = _UNSIGNED_BYTE
textureTypeToConst RGBA = _RGBA
textureTypeToConst FLOAT = _FLOAT
textureTypeToConst UNSIGNED_SHORT_5_6_5 = _UNSIGNED_SHORT_5_6_5
textureTypeToConst UNSIGNED_SHORT_4_4_4_4 = _UNSIGNED_SHORT_4_4_4_4
textureTypeToConst UNSIGNED_SHORT_5_5_5_1 = _UNSIGNED_SHORT_5_5_5_1

textureFor :: forall a eff. String -> (WebGLTexture -> EffWebGL eff a) -> EffWebGL eff Unit
textureFor name continuation = do
  texture <- createTexture
  loadImage name \image -> do
    handleLoad texture image
    continuation texture

handleLoad :: forall eff. WebGLTexture -> Image -> EffWebGL eff Unit
handleLoad texture image = do
  bindTexture _TEXTURE_2D texture
  pixelStorei _UNPACK_FLIP_Y_WEBGL 1
  texImage2D_ _TEXTURE_2D 0 _RGBA _RGBA _UNSIGNED_BYTE image
  texParameteri _TEXTURE_2D _TEXTURE_MAG_FILTER _NEAREST
  texParameteri _TEXTURE_2D _TEXTURE_MIN_FILTER _NEAREST
--  bindTexture _TEXTURE_2D 0

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
