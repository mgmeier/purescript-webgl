module Main where


import Control.Monad.Eff.WebGL
import Graphics.WebGL
import qualified Data.Matrix4 as M
import qualified Data.Matrix as M
import qualified Data.Vector3 as V3
import Control.Monad.Eff.Alert

import Control.Monad.Eff
import Debug.Trace
import Data.Tuple

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


main :: Eff (trace :: Trace, alert :: Alert) Unit
main = runWebGL "glcanvas" (\s -> alert s)
  \ context -> do
    trace "WebGL started"
    withShaders fshaderSource
                vshaderSource
                [VecAttr Three "aVertexPosition"]
                [Matrix Four "uPMatrix", Matrix Four "uMVMatrix"]
                (\s -> alert s)
      \ shaderProgram [aVertexPosition] [uPMatrix, uMVMatrix] -> do
        trace (show aVertexPosition.aLocation)
        clearColor 0.0 0.0 0.0 1.0
        enable DEPTH_TEST

        canvasWidth <- getCanvasWidth context
        canvasHeight <- getCanvasHeight context
        viewport 0 0 canvasWidth canvasHeight
        clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

        let pMatrix = M.makePerspective 45 (canvasWidth / canvasHeight) 0.1 100.0
        setMatrix4 uPMatrix pMatrix

        let mvMatrix = M.translate  (V3.vec3 (-1.5) 0.0 (-7.0)) M.identity
        setMatrix4 uMVMatrix mvMatrix

        buf1 <- makeBufferSimple [0.0,  1.0,  0.0,
                           (-1.0), (-1.0),  0.0,
                            1.0, (-1.0),  0.0]
        drawArr TRIANGLES buf1 aVertexPosition

        let mvMatrix' = M.translate (V3.vec3 3.0 0.0 0.0) mvMatrix
        setMatrix4 uMVMatrix mvMatrix'

        buf2 <- makeBufferSimple [1.0,  1.0,  0.0,
                           (-1.0), 1.0,  0.0,
                            1.0, (-1.0),  0.0,
                           (-1.0), (-1.0),  0.0]
        drawArr TRIANGLE_STRIP buf2 aVertexPosition

        trace "WebGL completed"
