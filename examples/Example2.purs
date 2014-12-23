module Main where

import Control.Monad.Eff.WebGL
import Control.Monad.Eff.WebGLRaw
import qualified Data.Matrix4 as M4
import qualified Data.Vector3 as V3
import Control.Monad.Eff.Alert

import Control.Monad.Eff
import Debug.Trace
import Data.Tuple

fshaderSource :: String
fshaderSource =
"""precision mediump float;

varying vec4 vColor;

void main(void) {
  gl_FragColor = vColor;
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
    (\s -> alert s)
      \ context -> do
        trace "WebGL started"
        withShaders fshaderSource
                    vshaderSource
                    [Vec3 "aVertexPosition", Vec4 "aVertexColor"]
                    [Mat4 "uPMatrix",Mat4 "uMVMatrix"]
                    (\s -> alert s)
                    \ shaderProgram [aVertexPosition, aVertexColor] [uPMatrix,uMVMatrix]-> do
          clearColor 0.0 0.0 0.0 1.0
          enable _DEPTH_TEST

          buf1 <- makeBufferSimple [0.0,  1.0,  0.0,
                              (-1.0), (-1.0),  0.0,
                              1.0, (-1.0),  0.0]
          buf1Colors <- makeBufferSimple  [
                              1.0, 0.0, 0.0, 1.0,
                              0.0, 1.0, 0.0, 1.0,
                              0.0, 0.0, 1.0, 1.0
                              ]
          buf2 <- makeBufferSimple [1.0,  1.0,  0.0,
                             (-1.0), 1.0,  0.0,
                              1.0, (-1.0),  0.0,
                             (-1.0), (-1.0),  0.0]
          buf2Colors <- makeBufferSimple
                             [0.5, 0.5, 1.0, 1.0,
                             0.5, 0.5, 1.0, 1.0,
                             0.5, 0.5, 1.0, 1.0,
                             0.5, 0.5, 1.0, 1.0]

          canvasWidth <- context.getCanvasWidth
          canvasHeight <- context.getCanvasHeight
          viewport 0 0 canvasWidth canvasHeight
          clear (_COLOR_BUFFER_BIT .|. _DEPTH_BUFFER_BIT)

          let pMatrix = M4.makePerspective 45 (canvasWidth / canvasHeight) 0.1 100.0
          setMatrix uPMatrix pMatrix
          let mvMatrix = M4.translate  (V3.vec3 (-1.5) 0.0 (-7.0)) M4.identity
          setMatrix uMVMatrix mvMatrix

          bindPointBuf buf1Colors aVertexColor
          drawArr buf1 aVertexPosition _TRIANGLES

          let mvMatrix' = M4.translate (V3.vec3 3.0 0.0 0.0) mvMatrix
          setMatrix uMVMatrix mvMatrix'

          bindPointBuf buf2Colors aVertexColor
          drawArr buf2 aVertexPosition _TRIANGLE_STRIP

          trace "WebGL completed"
