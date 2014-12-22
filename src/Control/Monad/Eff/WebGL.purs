-----------------------------------------------------------------------------
--
-- Module      :  Control.Monad.Eff.WebGL
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

module Control.Monad.Eff.WebGL
  (
    ShaderProgram(..)
  , EffWebGL(..)
  , ShaderType(..)

  , runWebGL
  , withShaders
  , setMatrix
  , makeBuffer
  , drawBuffer

  , vertexPointer
  , makeShader
  , initShaders
  , getCanvasWidth
  , getCanvasHeight

  ) where

import Control.Monad.Eff
import Control.Monad
import qualified Data.Matrix4 as M4
import Data.Maybe
import Data.Either
import qualified Data.StrMap as SMap

import Control.Monad.Eff.WebGLRaw
import Control.Monad.Eff.Alert
import Data.TypedArray

data ShaderType = FragmentShader
                  | VertexShader

type EffWebGL eff a = Eff (webgl :: WebGl | eff) a

type ShaderProgram = {
    glProgram :: WebGLProgram,
    attributes :: SMap.StrMap GLint,
    matrices :: SMap.StrMap WebGLUniformLocation
  }

drawBuffer :: forall eff. ShaderProgram -> WebGLBuffer -> String -> Number -> Number -> EffWebGL eff Unit
drawBuffer shaderProgram buf attrString typ size = do
  bindBuffer _ARRAY_BUFFER buf
  vertexPointer shaderProgram "aVertexPosition"
  drawArrays typ 0 size

vertexPointer ::  forall eff. ShaderProgram -> String -> EffWebGL eff Unit
vertexPointer prog key =
  case SMap.lookup key prog.attributes of
    Just loc -> vertexAttribPointer loc 3 _FLOAT false 0 0

setMatrix :: forall eff. ShaderProgram -> String -> M4.Mat4 -> EffWebGL eff Unit
setMatrix prog key mat =
  case SMap.lookup key prog.matrices of
    Just uni -> uniformMatrix4fv uni false (asArrayBuffer mat)


asArrayBuffer :: M4.Mat4 -> ArrayBuffer Float32
asArrayBuffer (M4.Mat4 v) = asFloat32Array v

-- | Returns either a (Left) continuation which takes a String in the error case,
--   which happens when WebGL is not present, or a (Right) continuation with the WebGL
--   effect.
runWebGL :: forall a eff. String -> (String -> Eff eff a) -> EffWebGL eff a -> Eff eff a
runWebGL canvasId failure success = do
  res <- initGL canvasId
  if res
    then runWebGl_ success
    else failure "Unable to initialize WebGL. Your browser may not support it."

withShaders :: forall a eff. String -> String -> [String] -> [String] -> (String -> EffWebGL eff a) ->
                (ShaderProgram -> EffWebGL eff a) -> EffWebGL eff a
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
                  attributeMap <- foldM (addAttribute p) SMap.empty attributes
                  matricesMap <- foldM (addMatrix p) SMap.empty matrices
                  success {glProgram : p,attributes : attributeMap, matrices : matricesMap}

addMatrix :: forall eff. WebGLProgram -> SMap.StrMap WebGLUniformLocation -> String
                  -> EffWebGL eff (SMap.StrMap WebGLUniformLocation)
addMatrix prog smap key = do
  val <- makeMatrix prog key
  return (SMap.insert key val smap)


makeMatrix :: forall eff. WebGLProgram -> String -> EffWebGL eff WebGLUniformLocation
makeMatrix prog name = do
 loc <- getUniformLocation prog name
 return loc

addAttribute :: forall eff. WebGLProgram -> SMap.StrMap GLint -> String -> EffWebGL eff (SMap.StrMap GLint)
addAttribute prog smap key = do
  val <- makeAttribute prog key
  return (SMap.insert key val smap)


makeAttribute :: forall eff. WebGLProgram -> String -> EffWebGL eff GLint
makeAttribute prog name = do
 loc <- getAttribLocation prog name
 enableVertexAttribArray loc
 return loc

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


makeBuffer :: forall eff. [Number] ->  Eff (webgl :: WebGl | eff) WebGLBuffer
makeBuffer vertices = do
  buffer <- createBuffer
  bindBuffer _ARRAY_BUFFER buffer
  let typedArray = asFloat32Array vertices
  bufferData _ARRAY_BUFFER typedArray _STATIC_DRAW
  return buffer


-- * Some hand writte foreign functions

foreign import runWebGl_ """
  function runWebGl_(f) {
      return f;
  }""" :: forall a e. Eff (webgl :: WebGl | e) a -> Eff e a

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
            var canvas = document.getElementById(canvasId);
            return canvas.width;
            } """ :: String -> Number

foreign import getCanvasHeight """
        function getCanvasHeight(canvasId) {
            var canvas = document.getElementById(canvasId);
            return canvas.height;
            } """ :: String -> Number
