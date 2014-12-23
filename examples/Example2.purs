module Main where

import Signal
import Signal.DOM
import Data.Maybe
import Data.Either
import Data.Array
import Data.Tuple
import Debug.Trace
import Control.Monad.Eff

import Control.Monad.Eff.WebGL
import Control.Monad.Eff.WebGLRaw
import Control.Monad.Eff.Alert
import qualified Data.Matrix4 as M4
import qualified Data.Vector3 as V3
import qualified Data.Vector as V

fshaderSource :: String
fshaderSource =
"""precision mediump float;

void main(void) {
  gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
"""

vshaderSource :: String
vshaderSource =
"""
    attribute vec3 aVertexPosition;
    attribute vec4 aVertexColor;

    uniform mat4 uMVMatrix;
    uniform mat4 uPMatrix;

    varying vec4 vColor;

    void main(void) {
        gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
        vColor = aVertexColor;
    }
"""

main :: Eff (trace :: Trace, alert :: Alert) Unit
main =
  runWebGL
    "glcanvas"
    (\s -> alert s) $ do
      trace "WebGL started"
      withShaders fshaderSource
                  vshaderSource
                  [Tuple "aVertexPosition" 3,Tuple "aVertexColor" 4]
                  ["uPMatrix","uMVMatrix"]
                  (\s -> alert s)
                  \ shaderProgram -> do
        clearColor 0.0 0.0 0.0 1.0
        enable _DEPTH_TEST

        buf1 <- makeBuffer [0.0,  1.0,  0.0,
                            (-1.0), (-1.0),  0.0,
                            1.0, (-1.0),  0.0]
        buf1Colors <- makeBuffer  [
                            1.0, 0.0, 0.0, 1.0,
                            0.0, 1.0, 0.0, 1.0,
                            0.0, 0.0, 1.0, 1.0
                            ]
        buf2 <- makeBuffer [1.0,  1.0,  0.0,
                           (-1.0), 1.0,  0.0,
                            1.0, (-1.0),  0.0,
                           (-1.0), (-1.0),  0.0]
        buf2Colors <- makeBuffer
                           [0.5, 0.5, 1.0, 1.0,
                           0.5, 0.5, 1.0, 1.0,
                           0.5, 0.5, 1.0, 1.0,
                           0.5, 0.5, 1.0, 1.0]

    --      viewport(0, 0, gl.viewportWidth, gl.viewportHeight);
        let canvasWidth = getCanvasWidth "glcanvas"
        let canvasHeight = getCanvasHeight "glcanvas"
        viewport 0 0 canvasWidth canvasHeight
        clear (_COLOR_BUFFER_BIT .|. _DEPTH_BUFFER_BIT)

        let pMatrix = M4.makePerspective 45 (canvasWidth / canvasHeight) 0.1 100.0
        setMatrix shaderProgram "uPMatrix" pMatrix

        let mvMatrix = M4.translate  (V3.vec3 (-1.5) 0.0 (-7.0)) M4.identity
        setMatrix shaderProgram "uMVMatrix" mvMatrix

        bindBuffer _ARRAY_BUFFER buf1Colors
        vertexPointer shaderProgram "aVertexColor"

        drawBuffer shaderProgram buf1 "aVertexPosition" _TRIANGLES 3


        let mvMatrix' = M4.translate (V3.vec3 3.0 0.0 0.0) mvMatrix
        setMatrix shaderProgram "uMVMatrix" mvMatrix'

        bindBuffer _ARRAY_BUFFER buf2Colors
        vertexPointer shaderProgram "aVertexColor"

        drawBuffer shaderProgram buf2 "aVertexPosition" _TRIANGLE_STRIP 4

        trace "WebGL completed"
