module Control.Monad.Eff.WebGL
  (
    runWebGL
  , withShaders

  , ShaderType(..)
  , makeShader
  , initShaders
  
  ) where

import Control.Monad.Eff
import Data.Maybe
import Data.Either

import Control.Monad.Eff.WebGLRaw
import Control.Monad.Eff.Alert

data ShaderType = FragmentShader
                  | VertexShader

-- | Returns either a (Left) continuation which takes a String in the error case,
--   which happens when WebGL is not present, or a (Right) continuation with the WebGL
--   effect.
runWebGL :: forall a eff. String -> (String -> Eff eff a) -> Eff (webgl :: WebGl | eff) a -> Eff eff a
runWebGL canvasId failure success = do
  res <- initGL canvasId
  if res
    then runWebGl_ success
    else failure "Unable to initialize WebGL. Your browser may not support it."

withShaders :: forall eff.String -> String -> Eff (webgl :: WebGl | eff) (Either WebGLProgram String)
withShaders fragmetShaderSource vertexShaderSource = do
  condFShader <- makeShader FragmentShader fragmetShaderSource -- (unlines fshaderSource)
  case condFShader of
    Nothing -> return $ Right "Can't compile fragment shader"
    Just fshader -> do
      condVShader <- makeShader VertexShader vertexShaderSource
      case condVShader of
        Nothing -> return $ Right "Can't compile vertex shader"
        Just vshader -> do
            condProg <- initShaders fshader vshader
            case condProg of
                Nothing -> return $ Right "Can't init shaders"
                Just p -> return $ Left p

makeShader :: forall eff. ShaderType -> String -> Eff (webgl :: WebGl | eff) (Maybe WebGLShader)
makeShader shaderType shaderSrc = do
  let shaderTypeConst = case shaderType of
                          FragmentShader -> _FRAGMENT_SHADER
                          VertexShader -> _VERTEX_SHADER
  shader <- createShader shaderTypeConst
  shaderSource shader shaderSrc
  compileShader shader
  res <- getShaderParameterBool shader _COMPILE_STATUS
  if res
      then return (Just shader)
      else return Nothing

initShaders :: forall eff. WebGLShader -> WebGLShader -> Eff (webgl :: WebGl | eff) (Maybe WebGLProgram)
initShaders fragmentShader vertexShader = do
  shaderProgram <- createProgram
  attachShader shaderProgram vertexShader
  attachShader shaderProgram fragmentShader
  linkProgram shaderProgram
  res <- getProgramParameterBool shaderProgram _LINK_STATUS
  if res
    then do
        useProgram shaderProgram
        return (Just shaderProgram)
    else return Nothing

-- * Foreign functions

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

foreign import getShaderParameterBool
  """function getShaderParameterBool(shader)
   {return function(pname)
    {return function()
     {return window.gl.getShaderParameter(shader,pname);};};};"""
    :: forall eff. WebGLShader->
                   GLenum
                   -> (Eff (webgl :: WebGl | eff) Boolean)

foreign import getProgramParameterBool
  """function getProgramParameterBool(program)
   {return function(pname)
    {return function()
     {return window.gl.getProgramParameter(program,pname);};};};"""
    :: forall eff. WebGLProgram->
                   GLenum
                   -> (Eff (webgl :: WebGl | eff) Boolean)



{--

foreign import glClearColor """
        function glClearColor(r) {
          return function(g) {
          return function(b) {
          return function(a) {
          return function()  {
            window.gl.clearColor(r,g,b,a);
        };};};};}""" :: forall eff.Number -> Number -> Number -> Number -> (Eff (eff) Unit)

foreign import glViewPort """
        function glViewPort(x) {
          return function(y) {
          return function(w) {
          return function(h) {
          return function()  {
            window.gl.viewport(x,y,w,h);
        };};};};}""" :: forall eff.Number -> Number -> Number -> Number -> (Eff (eff) Unit)

foreign import glClear """
        function glClear() {
            window.gl.clear(window.gl.COLOR_BUFFER_BIT | window.gl.DEPTH_BUFFER_BIT);
            window.gl.viewPortHeight = canvas.height;
        }""" :: forall eff.Eff (eff) Unit

foreign import getShader """
        function getShader(id) {
          return function() {
          var shaderScript = document.getElementById(id);
          if (!shaderScript) {
            return null;
          }
          var str = '';
          var k = shaderScript.firstChild;
          while (k) {
            if (k.nodeType == 3) {
              str += k.textContent;
            }
            k = k.nextSibling;
          }
          var shader;
          if (shaderScript.type == 'x-shader/x-fragment') {
            shader = window.gl.createShader(window.gl.FRAGMENT_SHADER);
          } else if (shaderScript.type == 'x-shader/x-vertex') {
            shader = window.gl.createShader(gl.VERTEX_SHADER);
          } else {
            return null;
          }
          console.log("shader:", shader);
          window.gl.shaderSource(shader, str);
          window.gl.compileShader(shader);
          return shader;
        };}""" :: forall eff.String -> (Eff (eff) GLShader)

foreign import glCreateProgram """
        function glCreateProgram() {
            return window.gl.createProgram();
        }""" :: forall eff.Eff (eff) GLProgram

foreign import glAttachShader """
        function glAttachShader(program) {
          return function(shader) {
          return function() {
            window.gl.attachShader(program, shader);
            console.log("as " ,program);
            return {};
        };};}""" :: forall eff.GLProgram -> GLShader -> Eff (eff) Unit

foreign import glLinkProgram """
        function glLinkProgram(program) {
          return function() {
            window.gl.linkProgram(program);
            console.log(program);
            return {};
        };}""" :: forall eff.GLProgram -> Eff (eff) Unit

foreign import glUseProgram """
        function glUseProgram(program) {
          return function() {
            window.gl.useProgram(program);
            return {};
        };}""" :: forall eff.GLProgram -> Eff (eff) Unit
--}
