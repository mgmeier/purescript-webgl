-----------------------------------------------------------------------------
--
-- Module      :  Graphics.WebGL
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

module Graphics.WebGL
  (
    WebGLContext(..)
  , VecBind(..)
  , MatBind(..)
  , VecDescr(..)
  , MatDescr(..)
  , Buffer(..)

  , runWebGL
  , withShaders
  , setMatrix
  , makeBuffer
  , makeBufferSimple
  , drawArr
  , bindPointBuf
  , bindBuf

  , requestAnimationFrame

  , vertexPointer

  ) where

import Control.Monad.Eff.WebGL
import Graphics.WebGLRaw
import qualified Data.Matrix4 as M4
import Data.TypedArray hiding (length)

import Control.Monad.Eff
import Control.Monad
import Data.Tuple
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array (reverse,length)

data ShaderType =   FragmentShader
                  | VertexShader

data VecDescr =  Vec2 String
                  | Vec3 String
                  | Vec4 String

type VecBind = {location :: GLint, itemSize :: Number, itemType :: Number}

data MatDescr =  Mat2 String
                  | Mat3 String
                  | Mat4 String

type MatBind = {location :: WebGLUniformLocation, itemSize :: Number, itemType :: Number}

type WebGLContext eff = {
    canvasName :: String,
    getCanvasWidth :: Eff (webgl :: WebGl | eff) Number,
    getCanvasHeight :: Eff (webgl :: WebGl | eff) Number
  }

type Buffer a = {
    webGLBuffer :: WebGLBuffer,
    bufferType  :: Number,
    bufferSize  :: Number
  }

setMatrix :: forall eff. MatBind -> M4.Mat4 -> EffWebGL eff Unit
setMatrix mb mat | mb.itemSize == 4 = uniformMatrix4fv mb.location false (asArrayBuffer mat)
setMatrix mb mat | mb.itemSize == 3 = uniformMatrix3fv mb.location false (asArrayBuffer mat)
setMatrix mb mat | mb.itemSize == 2 = uniformMatrix2fv mb.location false (asArrayBuffer mat)

asArrayBuffer :: M4.Mat4 -> ArrayBuffer Float32
asArrayBuffer (M4.Mat4 v) = asFloat32Array v


-- | Returns either a continuation which takes a String in the error case,
--   which happens when WebGL is not present, or a (Right) continuation with the WebGL
--   effect.
runWebGL :: forall a eff. String -> (String -> Eff eff a) -> (WebGLContext eff -> EffWebGL eff a) -> Eff eff a
runWebGL canvasId failure success = do
  res <- initGL canvasId
  if res
    then runWebGl_ (success makeContext)
    else failure "Unable to initialize WebGL. Your browser may not support it."
    where
      makeContext = {
          canvasName : canvasId,
          getCanvasWidth : getCanvasWidth canvasId,
          getCanvasHeight : getCanvasHeight canvasId
        }

withShaders :: forall a eff. String -> String -> [VecDescr] -> [MatDescr] -> (String -> EffWebGL eff a) ->
                (WebGLProgram -> [VecBind] -> [MatBind] -> EffWebGL eff a) -> EffWebGL eff a
withShaders fragmetShaderSource vertexShaderSource attributes matrices failure success = do
  condFShader <- makeShader FragmentShader fragmetShaderSource -- (unlines fshaderSource)
  case condFShader of
    Nothing -> failure "Can't compile fragment shader"
    Just fshader -> do
      condVShader <- makeShader VertexShader vertexShaderSource
      case condVShader of
        Nothing -> failure "Can't compile vertex shader"
        Just vshader -> do
            condProg <- initShaders fshader vshader
            case condProg of
                Nothing -> failure "Can't init shaders"
                Just p -> do
                  attributes <- foldM (addAttribute p) [] attributes
                  matrices <- foldM (addMatrix p) [] matrices
                  success p (reverse attributes) (reverse matrices)

addMatrix :: forall eff. WebGLProgram -> [MatBind] -> MatDescr -> EffWebGL eff [MatBind]
addMatrix prog list descr = do
  val <- makeMatrix prog descr
  return (val : list)

makeMatrix :: forall eff. WebGLProgram -> MatDescr -> EffWebGL eff MatBind
makeMatrix prog (Mat2 name) = do
 loc <- getUniformLocation prog name
 return {location : loc, itemSize : 2, itemType : _FLOAT}
makeMatrix prog (Mat3 name) = do
 loc <- getUniformLocation prog name
 return {location : loc, itemSize : 3, itemType : _FLOAT}
makeMatrix prog (Mat4 name) = do
 loc <- getUniformLocation prog name
 return {location : loc, itemSize : 4, itemType : _FLOAT}

addAttribute :: forall eff. WebGLProgram -> [VecBind] -> VecDescr
                    -> EffWebGL eff [VecBind]
addAttribute prog list descr = do
  bind <- makeAttribute prog descr
  return (bind : list)

makeAttribute :: forall eff. WebGLProgram -> VecDescr -> EffWebGL eff VecBind
makeAttribute prog (Vec2 name) = do
 loc <- getAttribLocation prog name
 enableVertexAttribArray loc
 return {location : loc, itemSize : 2, itemType : _FLOAT}
makeAttribute prog (Vec3 name) = do
 loc <- getAttribLocation prog name
 enableVertexAttribArray loc
 return {location : loc, itemSize : 3, itemType : _FLOAT}
makeAttribute prog (Vec4 name) = do
 loc <- getAttribLocation prog name
 enableVertexAttribArray loc
 return {location : loc, itemSize : 4, itemType : _FLOAT}

makeShader :: forall eff. ShaderType -> String -> Eff (webgl :: WebGl | eff) (Maybe WebGLShader)
makeShader shaderType shaderSrc = do
  let shaderTypeConst = case shaderType of
                          FragmentShader -> _FRAGMENT_SHADER
                          VertexShader -> _VERTEX_SHADER
  shader <- createShader shaderTypeConst
  shaderSource shader shaderSrc
  compileShader shader
  res <- getShaderParameter shader _COMPILE_STATUS
  if res
      then return (Just shader)
      else return Nothing

initShaders :: forall eff. WebGLShader -> WebGLShader -> Eff (webgl :: WebGl | eff) (Maybe WebGLProgram)
initShaders fragmentShader vertexShader = do
  shaderProgram <- createProgram
  attachShader shaderProgram vertexShader
  attachShader shaderProgram fragmentShader
  linkProgram shaderProgram
  res <- getProgramParameter shaderProgram _LINK_STATUS
  if res
    then do
        useProgram shaderProgram
        return (Just shaderProgram)
    else return Nothing

makeBufferSimple :: forall eff. [Number] ->  Eff (webgl :: WebGl | eff) (Buffer Float32)
makeBufferSimple vertices = do
  buffer <- createBuffer
  bindBuffer _ARRAY_BUFFER buffer
  let typedArray = asFloat32Array vertices
  bufferData _ARRAY_BUFFER typedArray _STATIC_DRAW
  return {
      webGLBuffer : buffer,
      bufferType  : _ARRAY_BUFFER,
      bufferSize  : length vertices
    }

makeBuffer :: forall a eff. Number -> ([Number] -> ArrayBuffer a) -> [Number]
                  ->  Eff (webgl :: WebGl | eff) (Buffer a)
makeBuffer arrayTyp conversion vertices = do
  buffer <- createBuffer
  bindBuffer arrayTyp buffer
  let typedArray = conversion vertices
  bufferData_ arrayTyp typedArray _STATIC_DRAW
  return {
      webGLBuffer : buffer,
      bufferType  : arrayTyp,
      bufferSize  : length vertices
    }

bindPointBuf :: forall a eff. Buffer a -> VecBind -> Eff (webgl :: WebGl | eff) Unit
bindPointBuf buffer bind = do
  bindBuffer buffer.bufferType buffer.webGLBuffer
  vertexPointer bind

bindBuf :: forall a eff. Buffer a -> Eff (webgl :: WebGl | eff) Unit
bindBuf buffer = bindBuffer buffer.bufferType buffer.webGLBuffer

vertexPointer ::  forall eff. VecBind -> EffWebGL eff Unit
vertexPointer vb = vertexAttribPointer vb.location vb.itemSize vb.itemType false 0 0

drawArr :: forall a eff. Buffer a -> VecBind -> Number -> EffWebGL eff Unit
drawArr buffer vecBind typ = do
  bindPointBuf buffer vecBind
  drawArrays typ 0 (buffer.bufferSize / vecBind.itemSize)

-- * Some hand writte foreign functions

foreign import initGL """
        function initGL(canvasId) {
          return function() {
            var canvas = document.getElementById(canvasId);
            try {
            window.gl = canvas.getContext("webgl") || canvas.getContext("experimental-webgl");
            }
            catch(e) {return false}
            if (!window.gl)
            {
              gl = null;
              return false;
            }
            return true;
            }
        }""" :: forall eff.String -> (Eff (eff) Boolean)

foreign import getCanvasWidth """
        function getCanvasWidth(canvasId) {
          return function() {
            var canvas = document.getElementById(canvasId);
            return canvas.width;
            };} """ ::  forall eff. String -> Eff (webgl :: WebGl | eff) Number

foreign import getCanvasHeight """
        function getCanvasHeight(canvasId) {
          return function() {
            var canvas = document.getElementById(canvasId);
            return canvas.height;
            };}""" :: forall eff. String -> Eff (webgl :: WebGl | eff) Number


foreign import requestAnimationFrame """
  if (typeof rAF === 'undefined') {
    var rAF = (function(){
      return  window.requestAnimationFrame       ||
              window.webkitRequestAnimationFrame ||
              window.mozRequestAnimationFrame    ||
              function( callback ){
                window.setTimeout(callback, 1000 / 60);
              };
    })();
  }
  function requestAnimationFrame(x){
    return function(){
      return rAF(x);
    };
  };
""" :: forall a eff. Eff (webgl :: WebGl | eff) a -> Eff (webgl :: WebGl | eff) Unit

foreign import bufferData_ """
    function bufferData_(target)
     {return function(data)
      {return function(usage)
       {return function()
        {window.gl.bufferData(target,data,usage);};};};};"""
      :: forall a eff. GLenum->
                     ArrayBuffer a ->
                     GLenum
                     -> (Eff (webgl :: WebGl | eff) Unit)
