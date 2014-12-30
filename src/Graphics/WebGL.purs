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
  ( WebGLContext(..)
  , WebGLProg(..)
  , VecBind(..)
  , MatBind(..)
  , VecDescr(..)
  , MatDescr(..)
  , Buffer(..)
  , Capacity(..)
  , Mask(..)
  , Mode(..)
  , BufferTarget(..)

  , getCanvasWidth
  , getCanvasHeight

  , viewport
  , enable
  , clearColor
  , clear

  , runWebGL
  , withShaders

  , makeBuffer
  , makeBufferSimple

  , setMatrix

  , bindPointBuf
  , bindBuf
  , vertexPointer

  , drawArr
  , drawElements

  , requestAnimationFrame

  ) where

import Control.Monad.Eff.WebGL
import Graphics.WebGLRaw
import qualified Data.Matrix4 as M4
import Data.TypedArray hiding (length)

import Control.Monad.Eff
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array (reverse,length,map)

data Capacity = BLEND
                  -- ^ Blend computed fragment color values with color buffer values.
                | DEPTH_TEST
                  -- ^Enable updates of the depth buffer.
                | CULL_FACE
                  -- ^ Let polygons be culled. See cullFace
                | POLYGON_OFFSET_FILL
                  -- ^ Add an offset to the depth values of a polygon's fragments.
                | SCISSOR_TEST
                  -- ^ Abandon fragments outside a scissor rectangle.

capacityToConst :: Capacity -> Number
capacityToConst BLEND = _BLEND
capacityToConst DEPTH_TEST = _DEPTH_TEST
capacityToConst CULL_FACE = _CULL_FACE
capacityToConst POLYGON_OFFSET_FILL = _POLYGON_OFFSET_FILL
capacityToConst SCISSOR_TEST = _SCISSOR_TEST

data Mask = DEPTH_BUFFER_BIT
                -- ^Clears the depth buffer	0x00000100
            | STENCIL_BUFFER_BIT
                -- ^Clears the stencil buffer	0x00000400
            | COLOR_BUFFER_BIT
                -- ^ Clears the color buffer	0x00004000

maskToConst :: Mask -> Number
maskToConst DEPTH_BUFFER_BIT   = _DEPTH_BUFFER_BIT
maskToConst STENCIL_BUFFER_BIT = _STENCIL_BUFFER_BIT
maskToConst COLOR_BUFFER_BIT   = _COLOR_BUFFER_BIT

data Mode = POINTS
              -- ^ Draws a single dot per vertex. For example, 10 vertices produce 10 dots.
          | LINES
              -- ^ Draws a line between a pair of vertices. For example, 10 vertices produce 5 separate lines.
          | LINE_STRIP
             -- ^ Draws a line to the next vertex by a straight line. For example, 10 vertices produce 9 lines connected end to end.
          | LINE_LOOP
             -- ^ Similar to gl.LINE_STRIP, but connects the last vertex back to the first. For example, 10 vertices produce 10 straight lines.
          | TRIANGLES
            -- ^ Draws a triangle for each group of three consecutive vertices. For example, 12 vertices create 4 separate triangles.
          | TRIANGLE_STRIP
            -- ^ Creates a strip of triangles where each additional vertex creates an additional triangle once the first three vertices have been drawn. For example, 12 vertices create 10 triangles.
          | TRIANGLE_FAN
            -- ^ Similar to gl.TRIANGLE_STRIP, but creates a fan shaped output. For example 12 vertices create 10 triangles.

modeToConst :: Mode -> Number
modeToConst POINTS = _POINTS
modeToConst LINES = _LINES
modeToConst LINE_STRIP = _LINE_STRIP
modeToConst LINE_LOOP = _LINE_LOOP
modeToConst TRIANGLES = _TRIANGLES
modeToConst TRIANGLE_STRIP = _TRIANGLE_STRIP
modeToConst TRIANGLE_FAN = _TRIANGLE_FAN

data BufferTarget = ARRAY_BUFFER
                    | ELEMENT_ARRAY_BUFFER

bufferTargetToConst ARRAY_BUFFER = _ARRAY_BUFFER
bufferTargetToConst ELEMENT_ARRAY_BUFFER = _ELEMENT_ARRAY_BUFFER

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

type WebGLContext = {
    canvasName :: String
  }

newtype WebGLProg = WebGLProg WebGLProgram

type Buffer a = {
    webGLBuffer :: WebGLBuffer,
    bufferType  :: Number,
    bufferSize  :: Number
  }

enable :: forall eff. Capacity -> (Eff (webgl :: WebGl | eff) Unit)
enable c = enable_ (capacityToConst c)

clear :: forall eff. [Mask] -> (Eff (webgl :: WebGl | eff) Unit)
clear masks = clear_ $ foldl (.|.) 0 (map maskToConst masks)

viewport = viewport_
clearColor = clearColor_

setMatrix :: forall eff. MatBind -> M4.Mat4 -> EffWebGL eff Unit
setMatrix mb mat | mb.itemSize == 4 = uniformMatrix4fv_ mb.location false (asArrayBuffer mat)
setMatrix mb mat | mb.itemSize == 3 = uniformMatrix3fv_ mb.location false (asArrayBuffer mat)
setMatrix mb mat | mb.itemSize == 2 = uniformMatrix2fv_ mb.location false (asArrayBuffer mat)

asArrayBuffer :: M4.Mat4 -> ArrayBuffer Float32
asArrayBuffer (M4.Mat4 v) = asFloat32Array v

getCanvasWidth :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Number
getCanvasWidth context = getCanvasWidth_ context.canvasName

getCanvasHeight :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Number
getCanvasHeight context = getCanvasHeight_ context.canvasName

-- | Returns either a continuation which takes a String in the error case,
--   which happens when WebGL is not present, or a (Right) continuation with the WebGL
--   effect.
runWebGL :: forall a eff. String -> (String -> Eff eff a) -> (WebGLContext -> EffWebGL eff a) -> Eff eff a
runWebGL canvasId failure success = do
  res <- initGL canvasId
  if res
    then runWebGl_ (success makeContext)
    else failure "Unable to initialize WebGL. Your browser may not support it."
    where
      makeContext = {
          canvasName : canvasId
        }

withShaders :: forall a eff. String -> String -> [VecDescr] -> [MatDescr] -> (String -> EffWebGL eff a) ->
                (WebGLProg -> [VecBind] -> [MatBind] -> EffWebGL eff a) -> EffWebGL eff a
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
                  success (WebGLProg p) (reverse attributes) (reverse matrices)

addMatrix :: forall eff. WebGLProgram -> [MatBind] -> MatDescr -> EffWebGL eff [MatBind]
addMatrix prog list descr = do
  val <- makeMatrix prog descr
  return (val : list)

makeMatrix :: forall eff. WebGLProgram -> MatDescr -> EffWebGL eff MatBind
makeMatrix prog (Mat2 name) = do
 loc <- getUniformLocation_ prog name
 return {location : loc, itemSize : 2, itemType : _FLOAT}
makeMatrix prog (Mat3 name) = do
 loc <- getUniformLocation_ prog name
 return {location : loc, itemSize : 3, itemType : _FLOAT}
makeMatrix prog (Mat4 name) = do
 loc <- getUniformLocation_ prog name
 return {location : loc, itemSize : 4, itemType : _FLOAT}

addAttribute :: forall eff. WebGLProgram -> [VecBind] -> VecDescr
                    -> EffWebGL eff [VecBind]
addAttribute prog list descr = do
  bind <- makeAttribute prog descr
  return (bind : list)

makeAttribute :: forall eff. WebGLProgram -> VecDescr -> EffWebGL eff VecBind
makeAttribute prog (Vec2 name) = do
 loc <- getAttribLocation_ prog name
 enableVertexAttribArray_ loc
 return {location : loc, itemSize : 2, itemType : _FLOAT}
makeAttribute prog (Vec3 name) = do
 loc <- getAttribLocation_ prog name
 enableVertexAttribArray_ loc
 return {location : loc, itemSize : 3, itemType : _FLOAT}
makeAttribute prog (Vec4 name) = do
 loc <- getAttribLocation_ prog name
 enableVertexAttribArray_ loc
 return {location : loc, itemSize : 4, itemType : _FLOAT}

makeShader :: forall eff. ShaderType -> String -> Eff (webgl :: WebGl | eff) (Maybe WebGLShader)
makeShader shaderType shaderSrc = do
  let shaderTypeConst = case shaderType of
                          FragmentShader -> _FRAGMENT_SHADER
                          VertexShader -> _VERTEX_SHADER
  shader <- createShader_ shaderTypeConst
  shaderSource_ shader shaderSrc
  compileShader_ shader
  res <- getShaderParameter_ shader _COMPILE_STATUS
  if res
      then return (Just shader)
      else return Nothing

initShaders :: forall eff. WebGLShader -> WebGLShader -> Eff (webgl :: WebGl | eff) (Maybe WebGLProgram)
initShaders fragmentShader vertexShader = do
  shaderProgram <- createProgram_
  attachShader_ shaderProgram vertexShader
  attachShader_ shaderProgram fragmentShader
  linkProgram_ shaderProgram
  res <- getProgramParameter_ shaderProgram _LINK_STATUS
  if res
    then do
        useProgram_ shaderProgram
        return (Just shaderProgram)
    else return Nothing

makeBufferSimple :: forall eff. [Number] ->  Eff (webgl :: WebGl | eff) (Buffer Float32)
makeBufferSimple vertices = do
  buffer <- createBuffer_
  bindBuffer_ _ARRAY_BUFFER buffer
  let typedArray = asFloat32Array vertices
  bufferData _ARRAY_BUFFER typedArray _STATIC_DRAW
  return {
      webGLBuffer : buffer,
      bufferType  : _ARRAY_BUFFER,
      bufferSize  : length vertices
    }

makeBuffer :: forall a eff. BufferTarget -> ([Number] -> ArrayBuffer a) -> [Number]
                  ->  Eff (webgl :: WebGl | eff) (Buffer a)
makeBuffer bufferTarget conversion vertices = do
  let targetConst = bufferTargetToConst bufferTarget
  buffer <- createBuffer_
  bindBuffer_ targetConst buffer
  let typedArray = conversion vertices
  bufferData targetConst typedArray _STATIC_DRAW
  return {
      webGLBuffer : buffer,
      bufferType  : targetConst,
      bufferSize  : length vertices
    }

bindPointBuf :: forall a eff. Buffer a -> VecBind -> Eff (webgl :: WebGl | eff) Unit
bindPointBuf buffer bind = do
  bindBuffer_ buffer.bufferType buffer.webGLBuffer
  vertexPointer bind

bindBuf :: forall a eff. Buffer a -> Eff (webgl :: WebGl | eff) Unit
bindBuf buffer = bindBuffer_ buffer.bufferType buffer.webGLBuffer

vertexPointer ::  forall eff. VecBind -> EffWebGL eff Unit
vertexPointer vb = vertexAttribPointer_ vb.location vb.itemSize vb.itemType false 0 0

drawArr :: forall a eff. Mode -> Buffer a -> VecBind -> EffWebGL eff Unit
drawArr mode buffer vecBind = do
  bindPointBuf buffer vecBind
  drawArrays_ (modeToConst mode) 0 (buffer.bufferSize / vecBind.itemSize)

drawElements :: forall a eff. Mode -> Number -> EffWebGL eff Unit
drawElements mode count = drawElements_ (modeToConst mode) count _UNSIGNED_SHORT 0

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

foreign import getCanvasWidth_ """
        function getCanvasWidth_(canvasId) {
          return function() {
            var canvas = document.getElementById(canvasId);
            return canvas.width;
            };} """ ::  forall eff. String -> Eff (webgl :: WebGl | eff) Number

foreign import getCanvasHeight_ """
        function getCanvasHeight_(canvasId) {
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

foreign import bufferData """
    function bufferData(target)
     {return function(data)
      {return function(usage)
       {return function()
        {window.gl.bufferData(target,data,usage);};};};};"""
      :: forall a eff. GLenum->
                     ArrayBuffer a ->
                     GLenum
                     -> (Eff (webgl :: WebGl | eff) Unit)
