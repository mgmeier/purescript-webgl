module Main where

import Signal
import Signal.DOM
import Data.Maybe
import Data.Either
import Debug.Trace
import Control.Monad.Eff

import Control.Monad.Eff.WebGL
import Control.Monad.Eff.WebGLRaw
import Control.Monad.Eff.Alert
import Data.TypedArray
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

    uniform mat4 uMVMatrix;
    uniform mat4 uPMatrix;

    void main(void) {
        gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
    }
"""

asArg :: M4.Mat4 -> ArrayBuffer Float32
asArg (M4.Mat4 v) = asFloat32Array v

main :: Eff (trace :: Trace, alert :: Alert) Unit
main =
  runWebGL
    "glcanvas"
    (\s -> alert s) $ do
      trace "WebGL started"
      withShaders fshaderSource
                  vshaderSource
                  ["aVertexPosition"]
                  ["uPMatrix","uMVMatrix"]
                  (\s -> alert s)
                  \ shaderProgram -> do
        clearColor 0.0 0.0 0.0 1.0
        enable _DEPTH_TEST


    --      viewport(0, 0, gl.viewportWidth, gl.viewportHeight);
        let canvasWidth = getCanvasWidth "glcanvas"
        let canvasHeight = getCanvasHeight "glcanvas"
        viewport 0 0 canvasWidth canvasHeight
        clear (_COLOR_BUFFER_BIT .|. _DEPTH_BUFFER_BIT)

        let pMatrix = M4.makePerspective 45 (canvasWidth / canvasHeight) 0.1 100.0
        setMatrix shaderProgram "uPMatrix" pMatrix

        let mvMatrix = M4.translate  (V3.vec3 (-1.5) 0.0 (-7.0)) M4.identity
        setMatrix shaderProgram "uMVMatrix" mvMatrix

        buf1 <- makeBuffer [0.0,  1.0,  0.0,
                           (-1.0), (-1.0),  0.0,
                            1.0, (-1.0),  0.0]
        drawBuffer shaderProgram buf1 "aVertexPosition" _TRIANGLES 3

        let mvMatrix' = M4.translate (V3.vec3 3.0 0.0 0.0) mvMatrix
        setMatrix shaderProgram "uMVMatrix" mvMatrix'

        buf2 <- makeBuffer [1.0,  1.0,  0.0,
                           (-1.0), 1.0,  0.0,
                            1.0, (-1.0),  0.0,
                           (-1.0), (-1.0),  0.0]
        drawBuffer shaderProgram buf2 "aVertexPosition" _TRIANGLE_STRIP 4

        trace "WebGL completed"
