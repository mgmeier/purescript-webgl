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
  , AttrLocation()
  , UniLocation()
  , Uniform(..)
  , Attr(..)
  , Size (..)
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

  , setUniformFloats
  , setUniformBooleans

  , bindPointBuf
  , bindBuf
  , vertexPointer

  , drawArr
  , drawElements

  , requestAnimationFrame

  ) where

import Control.Monad.Eff.WebGL
import Graphics.WebGLRaw
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.TypedArray as T
import Data.VecMat

import Control.Monad.Eff
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array (reverse,length,map,(!!))
import Data.Array.Unsafe (head)


data ShaderType =   FragmentShader
                  | VertexShader

data Size = One | Two | Three | Four

sizeToNumber :: Size -> Number
sizeToNumber One = 1
sizeToNumber Two = 2
sizeToNumber Three = 3
sizeToNumber Four = 4

data Uniform      = Float Size String
                  | Bool Size String
                  | Int Size String
                  | Vec Size  String
                  | VecInt Size String
                  | VecBool Size String
                  | Matrix Size String
                  | Sampler2D String

uniGetName :: Uniform -> String
uniGetName (Float _ s) = s
uniGetName (Bool _ s) = s
uniGetName (Int _ s) = s
uniGetName (Vec _  s) = s
uniGetName (VecInt _ s) = s
uniGetName (VecBool _ s) = s
uniGetName (Matrix _ s) = s
uniGetName (Sampler2D s) = s

uniGetSize :: Uniform -> Number
uniGetSize (Float i _) = sizeToNumber i
uniGetSize (Bool i _) = sizeToNumber i
uniGetSize (Int i _) = sizeToNumber i
uniGetSize (Vec i  _) = sizeToNumber i
uniGetSize (VecInt i _) = sizeToNumber i
uniGetSize (VecBool i _) = sizeToNumber i
uniGetSize (Matrix i _) = sizeToNumber i
uniGetSize (Sampler2D _) = 1

data Attr         = Attr Size String
                  | VecAttr Size String

attrGetName :: Attr -> String
attrGetName (Attr _ s) = s
attrGetName (VecAttr _ s) = s

attrGetSize :: Attr -> Number
attrGetSize (Attr i _) = sizeToNumber i
attrGetSize (VecAttr i _) = sizeToNumber i

attrItemType :: Attr -> Number
attrItemType _ = _FLOAT

type AttrLocation = {
    aLocation :: GLint,
    aAttr     :: Attr,
    aItemSize :: Number,
    aItemType :: Number}

type UniLocation = {
    uLocation :: WebGLUniformLocation,
    uUniform  :: Uniform}

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

setUniformFloats :: forall eff. UniLocation -> [Number] -> EffWebGL eff Unit
setUniformFloats uni value =
  case uni.uUniform of
    Matrix Four _     -> uniformMatrix4fv_ uni.uLocation false (asArrayBuffer value)
    Matrix Three _    -> uniformMatrix3fv_ uni.uLocation false (asArrayBuffer value)
    Matrix Two _      -> uniformMatrix2fv_ uni.uLocation false (asArrayBuffer value)
    Vec Four _        -> uniform4fv_ uni.uLocation (asArrayBuffer value)
    Vec Three _       -> uniform3fv_ uni.uLocation (asArrayBuffer value)
    Vec Two _         -> uniform2fv_ uni.uLocation (asArrayBuffer value)

setUniformBooleans :: forall eff. UniLocation -> [Boolean] -> EffWebGL eff Unit
setUniformBooleans uni value =
  case uni.uUniform of
    Bool One _    -> uniform1i_ uni.uLocation (head (toNumber <$> value))
    where
      toNumber true = 1
      toNumber false = 0

asArrayBuffer ::[Number] -> T.ArrayBuffer T.Float32
asArrayBuffer = T.asFloat32Array

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

withShaders :: forall a eff. String -> String -> [Attr] -> [Uniform] -> (String -> EffWebGL eff a) ->
                (WebGLProg -> [AttrLocation] -> [UniLocation] -> EffWebGL eff a) -> EffWebGL eff a
withShaders fragmetShaderSource vertexShaderSource attributes uniforms failure success = do
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
                  attributes' <- foldM (addAttribute p) [] attributes
                  uniforms' <- foldM (addUniform p) [] uniforms
                  success (WebGLProg p) (reverse attributes') (reverse uniforms')
  where
    addUniform :: forall eff. WebGLProgram -> [UniLocation] -> Uniform -> EffWebGL eff [UniLocation]
    addUniform prog list descr = do
      val <- makeUniform prog descr
      return (val : list)

    makeUniform :: forall eff. WebGLProgram -> Uniform -> EffWebGL eff UniLocation
    makeUniform prog uni = do
     loc <- getUniformLocation_ prog (uniGetName uni)
     return {uLocation : loc, uUniform: uni}

    addAttribute :: forall eff. WebGLProgram -> [AttrLocation] -> Attr
                        -> EffWebGL eff [AttrLocation]
    addAttribute prog list descr = do
      bind <- makeAttribute prog descr
      return (bind : list)

    makeAttribute :: forall eff. WebGLProgram -> Attr -> EffWebGL eff AttrLocation
    makeAttribute prog attr = do
     loc <- getAttribLocation_ prog (attrGetName attr)
     enableVertexAttribArray_ loc
     return {aLocation : loc, aAttr : attr, aItemSize : attrGetSize attr, aItemType : attrItemType attr}

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

makeBufferSimple :: forall eff. [Number] ->  Eff (webgl :: WebGl | eff) (Buffer T.Float32)
makeBufferSimple vertices = do
  buffer <- createBuffer_
  bindBuffer_ _ARRAY_BUFFER buffer
  let typedArray = T.asFloat32Array vertices
  bufferData _ARRAY_BUFFER typedArray _STATIC_DRAW
  return {
      webGLBuffer : buffer,
      bufferType  : _ARRAY_BUFFER,
      bufferSize  : length vertices
    }

makeBuffer :: forall a eff. BufferTarget -> ([Number] -> T.ArrayBuffer a) -> [Number]
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

bindPointBuf :: forall a eff. Buffer a -> AttrLocation -> Eff (webgl :: WebGl | eff) Unit
bindPointBuf buffer bind = do
  bindBuffer_ buffer.bufferType buffer.webGLBuffer
  vertexPointer bind

bindBuf :: forall a eff. Buffer a -> Eff (webgl :: WebGl | eff) Unit
bindBuf buffer = bindBuffer_ buffer.bufferType buffer.webGLBuffer

vertexPointer ::  forall eff. AttrLocation -> EffWebGL eff Unit
vertexPointer attrLoc =
  vertexAttribPointer_ attrLoc.aLocation attrLoc.aItemSize attrLoc.aItemType false 0 0

drawArr :: forall a eff. Mode -> Buffer a -> AttrLocation -> EffWebGL eff Unit
drawArr mode buffer attrLoc = do
  bindPointBuf buffer attrLoc
  drawArrays_ (modeToConst mode) 0 (buffer.bufferSize / attrLoc.aItemSize)

drawElements :: forall a eff. Mode -> Number -> EffWebGL eff Unit
drawElements mode count = drawElements_ (modeToConst mode) count _UNSIGNED_SHORT 0

-- * Constants

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
                     T.ArrayBuffer a ->
                     GLenum
                     -> (Eff (webgl :: WebGl | eff) Unit)
