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
    runWebGL
  , withShaders
  , EffWebGL(..)

  , ShaderType(..)
  , makeShader
  , initShaders

  , canvasWidth
  , canvasHeight

  ) where

import Control.Monad.Eff
import Data.Maybe
import Data.Either

import Control.Monad.Eff.WebGLRaw
import Control.Monad.Eff.Alert

data ShaderType = FragmentShader
                  | VertexShader

type EffWebGL eff a = Eff (webgl :: WebGl | eff) a

-- | Returns either a (Left) continuation which takes a String in the error case,
--   which happens when WebGL is not present, or a (Right) continuation with the WebGL
--   effect.
runWebGL :: forall a eff. String -> (String -> Eff eff a) -> EffWebGL eff a -> Eff eff a
runWebGL canvasId failure success = do
  res <- initGL canvasId
  if res
    then runWebGl_ success
    else failure "Unable to initialize WebGL. Your browser may not support it."

withShaders :: forall a eff. String -> String -> (String -> EffWebGL eff a) ->
                (WebGLProgram -> EffWebGL eff a) -> EffWebGL eff a
withShaders fragmetShaderSource vertexShaderSource failure success = do
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
                Just p -> success p

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

foreign import canvasWidth """
        function canvasWidth(canvasId) {
            var canvas = document.getElementById(canvasId);
            return canvas.width;
            } """ :: String -> Number

foreign import canvasHeight """
        function canvasHeight(canvasId) {
            var canvas = document.getElementById(canvasId);
            return canvas.height;
            } """ :: String -> Number
