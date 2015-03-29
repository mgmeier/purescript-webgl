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
  , ContextAttributes()
  , defContextAttributes
  , runWebGL
  , runWebGLAttr

  , Vec2()
  , Vec3()
  , Vec4()
  , Mat2()
  , Mat3()
  , Mat4()
  , Sampler2D()
  , Bool()
  , Float()

  , Uniform(..)
  , Attribute(..)
  , Shaders(..)
  , withShaders
  , WebGLProg()

  , Buffer(..)
  , BufferTarget(..)
  , makeBuffer
  , makeBufferFloat

  , setUniformFloats
  , setUniformBoolean

  , bindBufAndSetVertexAttr
  , bindBuf
  , vertexPointer

  , enableVertexAttribArray
  , disableVertexAttribArray
  , drawArr
  , drawElements

  , Mask(..)
  , Mode(..)

  , blendColor
  , blendFunc
  , blendFuncSeparate
  , blendEquation
  , blendEquationSeparate
  , BlendEquation(..)
  , BlendingFactor(..)

  , viewport
  , getCanvasWidth
  , getCanvasHeight

  , disable
  , enable
  , isEnabled
  , Capacity(..)

  , clear
  , clearColor
  , clearDepth
  , clearStencil
  , colorMask
  , isContextLost

  , requestAnimationFrame

  ) where

import Control.Monad.Eff.WebGL
import Graphics.WebGLRaw
import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Data.ArrayBuffer.Types as T
import qualified Data.TypedArray as T
import Data.TypeNat

import Control.Monad.Eff
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array (reverse,length,map,(!!))
import Data.Array.Unsafe (head)
import Data.Either

type WebGLContext = {
    canvasName :: String
  }

type ContextAttributes = { alpha :: Boolean
                         , depth :: Boolean
                         , stencil :: Boolean
                         , antialias :: Boolean
                         , premultipliedAlpha :: Boolean
                         , preserveDrawingBuffer :: Boolean
                         , preferLowPowerToHighPerformance :: Boolean
                         , failIfMajorPerformanceCaveat :: Boolean
                         }

defContextAttributes :: ContextAttributes
defContextAttributes = { alpha : true
                       , depth : true
                       , stencil : false
                       , antialias : true
                       , premultipliedAlpha : true
                       , preserveDrawingBuffer : false
                       , preferLowPowerToHighPerformance : false
                       , failIfMajorPerformanceCaveat : false
                       }

-- | Returns either a continuation which takes a String in the error case,
--   which happens when WebGL is not present, or a (Right) continuation with the WebGL
--   effect.
runWebGLAttr :: forall a eff. String -> ContextAttributes -> (String -> Eff eff a) -> (WebGLContext -> EffWebGL eff a) -> Eff eff a
runWebGLAttr canvasId attr failure success = do
  res <- initGL canvasId attr
  if res
    then runWebGl_ (success makeContext)
    else failure "Unable to initialize WebGL. Your browser may not support it."
    where
      makeContext = {
          canvasName : canvasId
        }

-- | Same as runWebGLAttr but uses default attributes (defContextAttributes)
runWebGL :: forall a eff. String -> (String -> Eff eff a) -> (WebGLContext -> EffWebGL eff a) -> Eff eff a
runWebGL canvasId failure success = do
  res <- initGL canvasId defContextAttributes
  if res
    then runWebGl_ (success makeContext)
    else failure "Unable to initialize WebGL. Your browser may not support it."
    where
      makeContext = {
          canvasName : canvasId
        }

newtype Uniform typ = Uniform
    {
      uLocation :: WebGLUniformLocation,
      uName     :: String,
      uType     :: Number
    }

newtype Attribute typ = Attribute {
    aLocation :: GLint,
    aName     :: String,
    aItemType :: Number,
    aItemSize :: Number}

data Vec2
data Vec3
data Vec4
data Mat2
data Mat3
data Mat4
data Sampler2D
data Bool
data Float

newtype WebGLProg = WebGLProg WebGLProgram

data Shaders bindings = Shaders String String

withShaders :: forall bindings eff a. Shaders (Object bindings) -> (String -> EffWebGL eff a) ->
                ({webGLProgram :: WebGLProg | bindings} -> EffWebGL eff a) -> EffWebGL eff a
withShaders (Shaders fragmetShaderSource vertexShaderSource) failure success = do
  condFShader <- makeShader FragmentShader fragmetShaderSource
  case condFShader of
    Right str -> failure ("Can't compile fragment shader: " ++ str)
    Left fshader -> do
      condVShader <- makeShader VertexShader vertexShaderSource
      case condVShader of
        Right str -> failure ("Can't compile vertex shader: " ++ str)
        Left vshader -> do
            condProg <- initShaders fshader vshader
            case condProg of
                Nothing ->
                  failure "Can't init shaders"
                Just p -> do
                  withBindings <- shaderBindings p
                  -- bindings2 <- checkBindings bindings1
                  success (withBindings{webGLProgram = WebGLProg p})


foreign import shaderBindings
  """
        function shaderBindings(program) {
          return function() {
            var bindings = {}
            var numUniforms = gl.getProgramParameter(program, gl.ACTIVE_UNIFORMS);
            for (var i = 0; i < numUniforms; i += 1) {
                var uniform = gl.getActiveUniform(program, i);
                var uniformLocation = gl.getUniformLocation(program, uniform.name);
                var newUniform = {};
                newUniform["uLocation"]=uniformLocation;
                newUniform["uName"]=uniform.name;
                newUniform["uType"]=uniform.type;
                bindings[uniform.name]=newUniform;
              }
            var numAttributes = gl.getProgramParameter(program, gl.ACTIVE_ATTRIBUTES);
            for (var i = 0; i < numAttributes; i += 1) {
                var attribute = gl.getActiveAttrib(program, i);
                var attribLocation = gl.getAttribLocation(program, attribute.name);
                gl.enableVertexAttribArray(attribLocation);
                var newAttr = {};
                newAttr["aLocation"]=attribLocation;
                newAttr["aName"]=attribute.name;
                newAttr["aItemType"]=attribute.type;
                switch (attribute.type) {
                  case gl.FLOAT_VEC2:
                    newAttr["aItemSize"]=2;
                    break;
                  case gl.FLOAT_VEC3:
                    newAttr["aItemSize"]=3;
                    break;
                  case gl.FLOAT_VEC4:
                    newAttr["aItemSize"]=4;
                    break;
                  default:
                    LOG("Unsupported attribute type: " + attribute.type);
                    newAttr["aItemSize"]=1;
                    break;
                }
                bindings[attribute.name]=newAttr;
            }
            return bindings;
            };}
  """ :: forall eff bindings. WebGLProgram -> Eff eff bindings

type Buffer a = {
    webGLBuffer :: WebGLBuffer,
    bufferType  :: Number,
    bufferSize  :: Number
  }

makeBufferFloat :: forall eff. [Number] ->  Eff (webgl :: WebGl | eff) (Buffer T.Float32)
makeBufferFloat vertices = do
  buffer <- createBuffer_
  bindBuffer_ _ARRAY_BUFFER buffer
  let typedArray = T.asFloat32Array vertices
  bufferData _ARRAY_BUFFER typedArray _STATIC_DRAW
  return {
      webGLBuffer : buffer,
      bufferType  : _ARRAY_BUFFER,
      bufferSize  : length vertices
    }

makeBuffer :: forall a eff. BufferTarget -> ([Number] -> T.ArrayView a) -> [Number]
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

setUniformFloats :: forall eff typ. Uniform typ -> [Number] -> EffWebGL eff Unit
setUniformFloats (Uniform uni) value
  | uni.uType == _FLOAT         = uniform1f_ uni.uLocation (head value)
  | uni.uType == _FLOAT_MAT4    = uniformMatrix4fv_ uni.uLocation false (asArrayBuffer value)
  | uni.uType == _FLOAT_MAT3    = uniformMatrix3fv_ uni.uLocation false (asArrayBuffer value)
  | uni.uType == _FLOAT_MAT2    = uniformMatrix2fv_ uni.uLocation false (asArrayBuffer value)
  | uni.uType == _FLOAT_VEC4    = uniform4fv_ uni.uLocation (asArrayBuffer value)
  | uni.uType == _FLOAT_VEC3    = uniform3fv_ uni.uLocation (asArrayBuffer value)
  | uni.uType == _FLOAT_VEC2    = uniform2fv_ uni.uLocation (asArrayBuffer value)

setUniformBoolean :: forall eff typ. Uniform typ -> Boolean -> EffWebGL eff Unit
setUniformBoolean (Uniform uni) value
  | uni.uType == _BOOL         = uniform1i_ uni.uLocation (toNumber value)
    where
      toNumber true = 1
      toNumber false = 0

bindBufAndSetVertexAttr :: forall a eff typ. Buffer a -> Attribute typ -> Eff (webgl :: WebGl | eff) Unit
bindBufAndSetVertexAttr buffer bind = do
  bindBuffer_ buffer.bufferType buffer.webGLBuffer
  vertexPointer bind

bindBuf :: forall a eff. Buffer a -> Eff (webgl :: WebGl | eff) Unit
bindBuf buffer = bindBuffer_ buffer.bufferType buffer.webGLBuffer

blendColor = blendColor_

blendFunc :: forall eff. BlendingFactor -> BlendingFactor -> (Eff (webgl :: WebGl | eff) Unit)
blendFunc a b = blendFunc_ (blendingFactorToConst a) (blendingFactorToConst b)

blendFuncSeparate :: forall eff. BlendingFactor
    -> BlendingFactor
    -> BlendingFactor
    -> BlendingFactor
    -> (Eff (webgl :: WebGl | eff) Unit)
blendFuncSeparate a b c d =
    let
        a' = blendingFactorToConst a
        b' = blendingFactorToConst b
        c' = blendingFactorToConst c
        d' = blendingFactorToConst d
    in blendFuncSeparate_ a' b' c' d'

blendEquation :: forall eff. BlendEquation -> (Eff (webgl :: WebGl | eff) Unit)
blendEquation = blendEquation_ <<< blendEquationToConst

blendEquationSeparate :: forall eff. BlendEquation -> BlendEquation -> (Eff (webgl :: WebGl | eff) Unit)
blendEquationSeparate a b = blendEquationSeparate_ (blendEquationToConst a) (blendEquationToConst b)

clear :: forall eff. [Mask] -> (Eff (webgl :: WebGl | eff) Unit)
clear masks = clear_ $ foldl (.|.) 0 (map maskToConst masks)

clearColor = clearColor_

clearDepth = clearDepth_

clearStencil = clearStencil_

colorMask = colorMask_

disable :: forall eff. Capacity -> (Eff (webgl :: WebGl | eff) Unit)
disable = disable_ <<< capacityToConst

drawArr :: forall a eff typ. Mode -> Buffer a -> Attribute typ -> EffWebGL eff Unit
drawArr mode buffer a@(Attribute attrLoc) = do
  bindBufAndSetVertexAttr buffer a
  drawArrays_ (modeToConst mode) 0 (buffer.bufferSize / attrLoc.aItemSize)

drawElements :: forall a eff. Mode -> Number -> EffWebGL eff Unit
drawElements mode count = drawElements_ (modeToConst mode) count _UNSIGNED_SHORT 0

enable :: forall eff. Capacity -> (Eff (webgl :: WebGl | eff) Unit)
enable = enable_ <<< capacityToConst

isContextLost = isContextLost_

isEnabled :: forall eff. Capacity -> (Eff (webgl :: WebGl | eff) Boolean)
isEnabled = isEnabled_ <<< capacityToConst

vertexPointer ::  forall eff typ. Attribute typ -> EffWebGL eff Unit
vertexPointer (Attribute attrLoc) =
  vertexAttribPointer_ attrLoc.aLocation attrLoc.aItemSize _FLOAT false 0 0

viewport = viewport_

enableVertexAttribArray :: forall eff a . Attribute a -> (Eff (webgl :: WebGl | eff) Unit)
enableVertexAttribArray (Attribute att)  = enableVertexAttribArray_ att.aLocation

disableVertexAttribArray :: forall eff a . Attribute a -> (Eff (webgl :: WebGl | eff) Unit)
disableVertexAttribArray (Attribute att) = disableVertexAttribArray_ att.aLocation



-- * Internal stuff

data ShaderType =   FragmentShader
                  | VertexShader

asArrayBuffer ::[Number] -> T.Float32Array
asArrayBuffer = T.asFloat32Array

getCanvasWidth :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Number
getCanvasWidth context = getCanvasWidth_ context.canvasName

getCanvasHeight :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Number
getCanvasHeight context = getCanvasHeight_ context.canvasName

makeShader :: forall eff. ShaderType -> String -> Eff (webgl :: WebGl | eff) (Either WebGLShader String)
makeShader shaderType shaderSrc = do
  let shaderTypeConst = case shaderType of
                          FragmentShader -> _FRAGMENT_SHADER
                          VertexShader -> _VERTEX_SHADER
  shader <- createShader_ shaderTypeConst
  shaderSource_ shader shaderSrc
  compileShader_ shader
  res <- getShaderParameter_ shader _COMPILE_STATUS
  if res
      then return (Left shader)
      else do
        str <- getShaderInfoLog_ shader
        return (Right str)

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


data BlendingFactor =
              ZERO
            | ONE
            | SRC_COLOR
            | ONE_MINUS_SRC_COLOR
            | DST_COLOR
            | ONE_MINUS_DST_COLOR
            | SRC_ALPHA
            | ONE_MINUS_SRC_ALPHA
            | DST_ALPHA
            | ONE_MINUS_DST_ALPHA
            | SRC_ALPHA_SATURATE
            | BLEND_DST_RGB
            | BLEND_SRC_RGB
            | BLEND_DST_ALPHA
            | BLEND_SRC_ALPHA
            | CONSTANT_COLOR
            | ONE_MINUS_CONSTANT_COLOR
            | CONSTANT_ALPHA
            | ONE_MINUS_CONSTANT_ALPHA
            | BLEND_COLOR


blendingFactorToConst :: BlendingFactor -> Number
blendingFactorToConst ZERO = _ZERO
blendingFactorToConst ONE = _ONE
blendingFactorToConst SRC_COLOR = _SRC_COLOR
blendingFactorToConst ONE_MINUS_SRC_COLOR = _ONE_MINUS_SRC_COLOR
blendingFactorToConst DST_COLOR = _DST_COLOR
blendingFactorToConst ONE_MINUS_DST_COLOR = _ONE_MINUS_DST_COLOR
blendingFactorToConst SRC_ALPHA = _SRC_ALPHA
blendingFactorToConst ONE_MINUS_SRC_ALPHA = _ONE_MINUS_SRC_ALPHA
blendingFactorToConst DST_ALPHA = _DST_ALPHA
blendingFactorToConst ONE_MINUS_DST_ALPHA = _ONE_MINUS_DST_ALPHA
blendingFactorToConst CONSTANT_COLOR = _CONSTANT_COLOR
blendingFactorToConst ONE_MINUS_CONSTANT_COLOR = _ONE_MINUS_CONSTANT_COLOR
blendingFactorToConst CONSTANT_ALPHA = _CONSTANT_ALPHA
blendingFactorToConst ONE_MINUS_CONSTANT_ALPHA = _ONE_MINUS_CONSTANT_ALPHA
blendingFactorToConst SRC_ALPHA_SATURATE = _SRC_ALPHA_SATURATE
blendingFactorToConst BLEND_COLOR = _BLEND_COLOR
blendingFactorToConst BLEND_DST_RGB = _BLEND_DST_RGB
blendingFactorToConst BLEND_SRC_RGB = _BLEND_SRC_RGB
blendingFactorToConst BLEND_DST_ALPHA = _BLEND_DST_ALPHA
blendingFactorToConst BLEND_SRC_ALPHA = _BLEND_SRC_ALPHA


data BlendEquation =
              FUNC_ADD
            | BLEND_EQUATION
            | BLEND_EQUATION_RGB
            | BLEND_EQUATION_ALPHA
            | FUNC_SUBTRACT
            | FUNC_REVERSE_SUBTRACT

blendEquationToConst :: BlendEquation -> Number
blendEquationToConst FUNC_ADD = _FUNC_ADD
blendEquationToConst BLEND_EQUATION = _BLEND_EQUATION
blendEquationToConst BLEND_EQUATION_RGB = _BLEND_EQUATION_RGB
blendEquationToConst BLEND_EQUATION_ALPHA = _BLEND_EQUATION_ALPHA
blendEquationToConst FUNC_SUBTRACT = _FUNC_SUBTRACT
blendEquationToConst FUNC_REVERSE_SUBTRACT = _FUNC_REVERSE_SUBTRACT


-- * Some hand written foreign functions

foreign import initGL """
        function initGL(canvasId) {
          return function(attr) {
            return function() {
              var canvas = document.getElementById(canvasId);
              try {
                gl = canvas.getContext("webgl", attr) || canvas.getContext("experimental-webgl", attr);
              }
              catch(e) {return false}
              if (!gl)
              {
                gl = null;
                return false;
              }
              return true;
              }
            }
        }""" :: forall eff. String -> ContextAttributes -> (Eff (eff) Boolean)

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

foreign import requestAnimationFrame_ """
  var rAF = null;

  function requestAnimationFrame_(getContext){
    return function(x){
      if(!rAF){
        rAF = (function(){
          var c = getContext();
          return  c.requestAnimationFrame       ||
                  c.webkitRequestAnimationFrame ||
                  c.mozRequestAnimationFrame    ||
                  c.oRequestAnimationFrame ||
                  c.msRequestAnimationFrame ||
                  function( callback ){
                    c.setTimeout(callback, 1000 / 60);
                  };
        })();
      }
      return function(){ return rAF(x); };
    }
  };
""" :: forall a eff. Eff eff Context -> Eff (webgl :: WebGl | eff) a -> Eff (webgl :: WebGl | eff) Unit

requestAnimationFrame :: forall a eff. Eff (webgl :: WebGl | eff) a -> Eff (webgl :: WebGl | eff) Unit
requestAnimationFrame =  requestAnimationFrame_ getContext

foreign import data Context :: *

foreign import getContext """
  var context;
  try      { context = Function('return this')() || (42, eval)('this'); }
  catch(e) { context = window; }
  function getContext(){ return context; }
""" :: forall e. Eff e Context

foreign import bufferData """
    function bufferData(target)
     {return function(data)
      {return function(usage)
       {return function()
        {gl.bufferData(target,data,usage);};};};};"""
      :: forall a eff. GLenum->
                     T.ArrayView a ->
                     GLenum
                     -> (Eff (webgl :: WebGl | eff) Unit)
