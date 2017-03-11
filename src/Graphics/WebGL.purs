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
  , makeBufferDyn
  , makeBufferFloat
  , makeBufferFloatDyn
  , makeBufferPrim
  , makeBufferPrimDyn
  , fillBuffer

  , setUniformFloats
  , setUniformBoolean

  , bindBufAndSetVertexAttr
  , bindBuf
  , bindAttribLocation
  , vertexPointer

  , enableVertexAttribArray
  , disableVertexAttribArray
  , drawArr
  , drawElements

  , depthFunc
  , Func(..)

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

import Prelude
import Control.Monad.Eff (Eff)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Array.Partial (head)
import Data.Array (length)
import Data.TypedArray (length) as AW
import Data.Either (Either(Right, Left))
import Data.Int.Bits ((.|.))
import Partial.Unsafe (unsafePartial)

import Control.Monad.Eff.WebGL (WebGl, EffWebGL, runWebGl_)
import Graphics.WebGLRaw (GLintptr, GLenum, WebGLProgram, WebGLShader, GLsizei, GLint, GLboolean, GLclampf, WebGLBuffer, WebGLUniformLocation,
     _FUNC_REVERSE_SUBTRACT, _FUNC_SUBTRACT, _BLEND_EQUATION_ALPHA, _BLEND_EQUATION_RGB, _BLEND_EQUATION, _FUNC_ADD, _BLEND_SRC_ALPHA, _BLEND_DST_ALPHA,
     _BLEND_SRC_RGB, _BLEND_DST_RGB, _BLEND_COLOR, _SRC_ALPHA_SATURATE, _ONE_MINUS_CONSTANT_ALPHA, _CONSTANT_ALPHA, _ONE_MINUS_CONSTANT_COLOR, _CONSTANT_COLOR,
     _ONE_MINUS_DST_ALPHA, _DST_ALPHA, _ONE_MINUS_SRC_ALPHA, _SRC_ALPHA, _ONE_MINUS_DST_COLOR, _DST_COLOR, _ONE_MINUS_SRC_COLOR, _SRC_COLOR, _ONE, _ZERO,
     _ELEMENT_ARRAY_BUFFER, _ARRAY_BUFFER, _TRIANGLE_FAN, _TRIANGLE_STRIP, _TRIANGLES, _LINE_LOOP, _LINE_STRIP, _LINES, _POINTS, _COLOR_BUFFER_BIT,
     _STENCIL_BUFFER_BIT, _DEPTH_BUFFER_BIT, _SCISSOR_TEST, _POLYGON_OFFSET_FILL, _CULL_FACE, _DEPTH_TEST, _BLEND, useProgram_, _LINK_STATUS,
     getProgramParameter_, linkProgram_, attachShader_, createProgram_, getShaderInfoLog_, _COMPILE_STATUS, getShaderParameter_, compileShader_,
     shaderSource_, createShader_, _VERTEX_SHADER, _FRAGMENT_SHADER, disableVertexAttribArray_, enableVertexAttribArray_, viewport_, _FLOAT,
     vertexAttribPointer_, isEnabled_, isContextLost_, enable_, _UNSIGNED_SHORT, drawElements_, drawArrays_, disable_, depthFunc_, _NOTEQUAL,
     _GEQUAL, _GREATER, _LEQUAL, _EQUAL, _LESS, _ALWAYS, _NEVER, colorMask_, clearStencil_, clearDepth_, clearColor_, clear_, blendEquationSeparate_,
     blendEquation_, blendFuncSeparate_, blendFunc_, blendColor_, bindBuffer_, uniform1i_, _BOOL, uniform2fv_, _FLOAT_VEC2, uniform3fv_, _FLOAT_VEC3,
     uniform4fv_, _FLOAT_VEC4, uniformMatrix2fv_, _FLOAT_MAT2, uniformMatrix3fv_, _FLOAT_MAT3, uniformMatrix4fv_, _FLOAT_MAT4, uniform1f_,
     createBuffer_, _DYNAMIC_DRAW, _STATIC_DRAW, bindAttribLocation_)
import Data.ArrayBuffer.Types (ArrayView, Float32Array, Float32) as T
import Data.TypedArray (asFloat32Array) as T
import Extensions (fail)

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

-- | pures either a continuation which takes a String in the error case,
--   which happens when WebGL is not present, or a (Right) continuation with the WebGL
--   effect.
runWebGLAttr :: forall a eff. String -> ContextAttributes -> (String -> Eff eff a) -> (WebGLContext -> EffWebGL eff a) -> Eff eff a
runWebGLAttr canvasId attr failure success = do
  res <- initGL_ canvasId attr
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
  res <- initGL_ canvasId defContextAttributes
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
      uType     :: Int
    }

newtype Attribute typ = Attribute {
    aLocation :: GLint,
    aName     :: String,
    aItemType :: Int,
    aItemSize :: Int}

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

requestAnimationFrame :: forall a eff. Eff (webgl :: WebGl | eff) a -> Eff (webgl :: WebGl | eff) Unit
requestAnimationFrame = requestAnimationFrame_

withShaders :: forall bindings eff a. Shaders (Record bindings) -> (String -> EffWebGL eff a) ->
                ({webGLProgram :: WebGLProg | bindings} -> EffWebGL eff a) -> EffWebGL eff a
withShaders (Shaders fragmetShaderSource vertexShaderSource) failure success = do
  condFShader <- makeShader FragmentShader fragmetShaderSource
  case condFShader of
    Right str -> failure ("Can't compile fragment shader: " <> str)
    Left fshader -> do
      condVShader <- makeShader VertexShader vertexShaderSource
      case condVShader of
        Right str -> failure ("Can't compile vertex shader: " <> str)
        Left vshader -> do
            condProg <- initShaders fshader vshader
            case condProg of
                Nothing ->
                  failure "Can't init shaders"
                Just p -> do
                  withBindings <- shaderBindings_ p
                  -- bindings2 <- checkBindings bindings1
                  success (withBindings{webGLProgram = WebGLProg p})

bindAttribLocation :: forall eff. WebGLProg -> Int -> String -> Eff (webgl :: WebGl | eff) Unit
bindAttribLocation (WebGLProg p) i s = bindAttribLocation_ p i s

type Buffer a = {
    webGLBuffer :: WebGLBuffer,
    bufferType  :: Int,
    bufferSize  :: Int
  }

makeBufferFloat :: forall eff. Array Number ->  Eff (webgl :: WebGl | eff) (Buffer T.Float32)
makeBufferFloat vertices = makeBufferFloat' vertices _STATIC_DRAW

makeBufferFloatDyn :: forall eff. Array Number ->  Eff (webgl :: WebGl | eff) (Buffer T.Float32)
makeBufferFloatDyn vertices = makeBufferFloat' vertices _DYNAMIC_DRAW

makeBufferFloat' :: forall eff. Array Number ->  Int -> Eff (webgl :: WebGl | eff) (Buffer T.Float32)
makeBufferFloat' vertices flag = do
  buffer <- createBuffer_
  bindBuffer_ _ARRAY_BUFFER buffer
  let typedArray = T.asFloat32Array vertices
  bufferData__ _ARRAY_BUFFER typedArray flag
  pure {
      webGLBuffer : buffer,
      bufferType  : _ARRAY_BUFFER,
      bufferSize  : length vertices
    }

makeBuffer :: forall a eff num. (EuclideanRing num) => BufferTarget -> (Array num -> T.ArrayView a) -> Array num
                  ->  Eff (webgl :: WebGl | eff) (Buffer a)
makeBuffer bufferTarget conversion vertices = makeBuffer' bufferTarget conversion vertices _STATIC_DRAW

makeBufferDyn :: forall a eff num. (EuclideanRing num) =>  BufferTarget -> (Array num -> T.ArrayView a) -> Array num
                  ->  Eff (webgl :: WebGl | eff) (Buffer a)
makeBufferDyn bufferTarget conversion vertices = makeBuffer' bufferTarget conversion vertices _DYNAMIC_DRAW

makeBufferPrim :: forall a eff.BufferTarget -> T.ArrayView a ->  Eff (webgl :: WebGl | eff) (Buffer a)
makeBufferPrim bufferTarget typedArray = do
  let targetConst = bufferTargetToConst bufferTarget
  buffer <- createBuffer_
  bindBuffer_ targetConst buffer
  bufferData__ targetConst typedArray _STATIC_DRAW
  pure {
      webGLBuffer : buffer,
      bufferType  : targetConst,
      bufferSize  : AW.length typedArray
    }

makeBufferPrimDyn :: forall a eff. BufferTarget -> T.ArrayView a ->  Eff (webgl :: WebGl | eff) (Buffer a)
makeBufferPrimDyn bufferTarget typedArray = do
  let targetConst = bufferTargetToConst bufferTarget
  buffer <- createBuffer_
  bindBuffer_ targetConst buffer
  bufferData__ targetConst typedArray _DYNAMIC_DRAW
  pure {
      webGLBuffer : buffer,
      bufferType  : targetConst,
      bufferSize  : AW.length typedArray
    }

makeBuffer' :: forall a eff num. (EuclideanRing num) => BufferTarget -> (Array num -> T.ArrayView a) -> Array num
                  ->  Int -> Eff (webgl :: WebGl | eff) (Buffer a)
makeBuffer' bufferTarget conversion vertices flag = do
  let targetConst = bufferTargetToConst bufferTarget
  buffer <- createBuffer_
  bindBuffer_ targetConst buffer
  let typedArray = conversion vertices
  bufferData__ targetConst typedArray flag
  pure {
      webGLBuffer : buffer,
      bufferType  : targetConst,
      bufferSize  : length vertices
    }

fillBuffer :: forall a eff. Buffer a -> Int -> Array Number -> Eff (webgl :: WebGl | eff) Unit
fillBuffer buffer offset vertices = do
    bindBuffer_ buffer.bufferType buffer.webGLBuffer
    let typedArray = T.asFloat32Array vertices
    bufferSubData__ buffer.bufferType offset typedArray
    pure unit


setUniformFloats :: forall eff typ. Uniform typ -> Array Number -> EffWebGL eff Unit
setUniformFloats (Uniform uni) value
  | uni.uType == _FLOAT         = uniform1f_ uni.uLocation (unsafePartial $ head value)
  | uni.uType == _FLOAT_MAT4    = uniformMatrix4fv_ uni.uLocation false (asArrayBuffer value)
  | uni.uType == _FLOAT_MAT3    = uniformMatrix3fv_ uni.uLocation false (asArrayBuffer value)
  | uni.uType == _FLOAT_MAT2    = uniformMatrix2fv_ uni.uLocation false (asArrayBuffer value)
  | uni.uType == _FLOAT_VEC4    = uniform4fv_ uni.uLocation (asArrayBuffer value)
  | uni.uType == _FLOAT_VEC3    = uniform3fv_ uni.uLocation (asArrayBuffer value)
  | uni.uType == _FLOAT_VEC2    = uniform2fv_ uni.uLocation (asArrayBuffer value)
  | otherwise                   = fail "WebGL>>setUniformFloats: Called for non float uniform!"

setUniformBoolean :: forall eff typ. Uniform typ -> Boolean -> EffWebGL eff Unit
setUniformBoolean (Uniform uni) value
  | uni.uType == _BOOL         = uniform1i_ uni.uLocation (toNumber value)
    where
      toNumber true = 1
      toNumber false = 0
  | otherwise                   = fail "WebGL>>setUniformBoolean: Called for not boolean uniform!"

bindBufAndSetVertexAttr :: forall a eff typ. Buffer a -> Attribute typ -> Eff (webgl :: WebGl | eff) Unit
bindBufAndSetVertexAttr buffer attr = do
    bindBuffer_ buffer.bufferType buffer.webGLBuffer
    vertexPointer attr


bindBuf :: forall a eff. Buffer a -> Eff (webgl :: WebGl | eff) Unit
bindBuf buffer = bindBuffer_ buffer.bufferType buffer.webGLBuffer

blendColor :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit
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

clear :: forall eff. Array Mask -> (Eff (webgl :: WebGl | eff) Unit)
clear masks = clear_ $ foldl (.|.) 0 (map maskToConst masks)

clearColor :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit
clearColor = clearColor_

clearDepth :: forall eff. GLclampf -> Eff (webgl :: WebGl | eff) Unit
clearDepth = clearDepth_

clearStencil :: forall eff. GLint -> Eff (webgl :: WebGl | eff) Unit
clearStencil = clearStencil_

colorMask :: forall eff. GLboolean -> GLboolean -> GLboolean -> GLboolean -> Eff (webgl :: WebGl | eff) Unit
colorMask = colorMask_

data Func = NEVER | ALWAYS | LESS | EQUAL | LEQUAL | GREATER | GEQUAL | NOTEQUAL

funcToConst :: Func -> Int
funcToConst NEVER   = _NEVER
funcToConst ALWAYS  = _ALWAYS
funcToConst LESS    = _LESS
funcToConst EQUAL   = _EQUAL
funcToConst LEQUAL  = _LEQUAL
funcToConst GREATER = _GREATER
funcToConst GEQUAL  = _GEQUAL
funcToConst NOTEQUAL = _NOTEQUAL

depthFunc :: forall eff. Func -> Eff (webgl :: WebGl | eff) Unit
depthFunc = depthFunc_ <<< funcToConst

disable :: forall eff. Capacity -> (Eff (webgl :: WebGl | eff) Unit)
disable = disable_ <<< capacityToConst

drawArr :: forall a eff typ. Mode -> Buffer a -> Attribute typ -> EffWebGL eff Unit
drawArr mode buffer a@(Attribute attrLoc) = do
  bindBufAndSetVertexAttr buffer a
  drawArrays_ (modeToConst mode) 0 (buffer.bufferSize / attrLoc.aItemSize)

drawElements :: forall eff. Mode -> Int -> EffWebGL eff Unit
drawElements mode count = drawElements_ (modeToConst mode) count _UNSIGNED_SHORT 0

enable :: forall eff. Capacity -> (Eff (webgl :: WebGl | eff) Unit)
enable = enable_ <<< capacityToConst

isContextLost :: forall eff. Eff (webgl :: WebGl | eff) Boolean
isContextLost = isContextLost_

isEnabled :: forall eff. Capacity -> (Eff (webgl :: WebGl | eff) Boolean)
isEnabled = isEnabled_ <<< capacityToConst

vertexPointer ::  forall eff typ. Attribute typ -> EffWebGL eff Unit
vertexPointer (Attribute attrLoc) =
  vertexAttribPointer_ attrLoc.aLocation attrLoc.aItemSize _FLOAT false 0 0

viewport :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit
viewport = viewport_

enableVertexAttribArray :: forall eff a . Attribute a -> (Eff (webgl :: WebGl | eff) Unit)
enableVertexAttribArray (Attribute att)  = enableVertexAttribArray_ att.aLocation

disableVertexAttribArray :: forall eff a . Attribute a -> (Eff (webgl :: WebGl | eff) Unit)
disableVertexAttribArray (Attribute att) = disableVertexAttribArray_ att.aLocation



-- * Internal stuff

data ShaderType =   FragmentShader
                  | VertexShader

asArrayBuffer ::Array Number -> T.Float32Array
asArrayBuffer = T.asFloat32Array

getCanvasWidth :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Int
getCanvasWidth context = getCanvasWidth_ context.canvasName

getCanvasHeight :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Int
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
      then pure (Left shader)
      else do
        str <- getShaderInfoLog_ shader
        pure (Right str)

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
        pure (Just shaderProgram)
    else pure Nothing



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

capacityToConst :: Capacity -> Int
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

maskToConst :: Mask -> Int
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

modeToConst :: Mode -> Int
modeToConst POINTS = _POINTS
modeToConst LINES = _LINES
modeToConst LINE_STRIP = _LINE_STRIP
modeToConst LINE_LOOP = _LINE_LOOP
modeToConst TRIANGLES = _TRIANGLES
modeToConst TRIANGLE_STRIP = _TRIANGLE_STRIP
modeToConst TRIANGLE_FAN = _TRIANGLE_FAN


data BufferTarget = ARRAY_BUFFER
                    | ELEMENT_ARRAY_BUFFER

bufferTargetToConst :: BufferTarget -> Int
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


blendingFactorToConst :: BlendingFactor -> Int
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

blendEquationToConst :: BlendEquation -> Int
blendEquationToConst FUNC_ADD = _FUNC_ADD
blendEquationToConst BLEND_EQUATION = _BLEND_EQUATION
blendEquationToConst BLEND_EQUATION_RGB = _BLEND_EQUATION_RGB
blendEquationToConst BLEND_EQUATION_ALPHA = _BLEND_EQUATION_ALPHA
blendEquationToConst FUNC_SUBTRACT = _FUNC_SUBTRACT
blendEquationToConst FUNC_REVERSE_SUBTRACT = _FUNC_REVERSE_SUBTRACT


-- * Some hand written foreign functions


foreign import shaderBindings_ :: forall eff bindings. WebGLProgram -> Eff eff bindings

foreign import initGL_ :: forall eff. String -> ContextAttributes -> Eff (eff) Boolean

foreign import getCanvasWidth_ ::  forall eff. String -> Eff (webgl :: WebGl | eff) Int

foreign import getCanvasHeight_ :: forall eff. String -> Eff (webgl :: WebGl | eff) Int

foreign import requestAnimationFrame_ :: forall a eff. Eff (webgl :: WebGl | eff) a -> Eff (webgl :: WebGl | eff) Unit

foreign import bufferData__ :: forall a eff. GLenum
                   -> T.ArrayView a
                   -> GLenum
                   -> Eff (webgl :: WebGl | eff) Unit

foreign import bufferSubData__ :: forall a eff. GLenum
                   -> GLintptr
                   -> T.ArrayView a
                   -> Eff (webgl :: WebGl | eff) Unit
