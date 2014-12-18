module Main where

import Signal
import Signal.DOM
import Data.Maybe
import Data.Either
import Debug.Trace
import Control.Monad.Eff

import Control.Monad.Eff.WebGL
import Control.Monad.Eff.WebGLRaw (WebGl(..), WebGLShader(..), viewport, createProgram, attachShader, linkProgram, clearColor
  ,enable, _DEPTH_TEST, clear, _COLOR_BUFFER_BIT, _DEPTH_BUFFER_BIT)
import Control.Monad.Eff.Alert

main :: Eff (trace :: Trace, alert :: Alert) Unit
main = runWebGL "glcanvas" (\s -> alert s) $ do
  trace "WebGL started"
  res <- withShaders fshaderSource vshaderSource
  case res of
    Right errString -> alert errString
    Left shaderProgram -> do
      viewport 0 0 5.0 5.0
      clearColor 0.0 1.0 0.0 1.0
      enable _DEPTH_TEST
      clear (_COLOR_BUFFER_BIT .|. _DEPTH_BUFFER_BIT)
      trace "WebGL completed"
      return unit


{-
  shaderProgram.vertexPositionAttribute = gl.getAttribLocation(shaderProgram, "aVertexPosition");
  gl.enableVertexAttribArray(shaderProgram.vertexPositionAttribute);

  shaderProgram.pMatrixUniform = gl.getUniformLocation(shaderProgram, "uPMatrix");
  shaderProgram.mvMatrixUniform = gl.getUniformLocation(shaderProgram, "uMVMatrix");
-}



fshaderSource =
"""precision mediump float;

void main(void) {
  gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
"""

vshaderSource =
"""attribute vec3 aVertexPosition;

    uniform mat4 uMVMatrix;
    uniform mat4 uPMatrix;

    void main(void) {
        gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
    }
"""
