module Main where

import Control.Monad.Eff.WebGL
import Control.Monad.Eff.WebGLRaw
import qualified Data.Matrix4 as M4
import qualified Data.Vector3 as V3
import Control.Monad.Eff.Alert

import Control.Monad.Eff
import Control.Monad
import Debug.Trace
import Data.Tuple
import Data.Date
import Data.Maybe
import Math

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

type State eff = {
                    context :: WebGLContext eff,
                    shaderProgram :: WebGLProgram,
                    aVertexPosition :: AttributeBinding,
                    aVertexColor  :: AttributeBinding,
                    uPMatrix :: MatrixBinding,
                    uMVMatrix :: MatrixBinding,
                    buf1 ::WebGLBuffer,
                    buf1Colors :: WebGLBuffer,
                    buf2 :: WebGLBuffer,
                    buf2Colors :: WebGLBuffer,
                    lastTime :: Maybe Number,
                    rTri :: Number,
                    rSquare :: Number
                }

main :: Eff (trace :: Trace, alert :: Alert, now :: Now) Unit
main =
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        trace "WebGL started"
        withShaders fshaderSource
                    vshaderSource
                    [Tuple "aVertexPosition" 3, Tuple "aVertexColor" 4]
                    ["uPMatrix","uMVMatrix"]
                    (\s -> alert s)
                      \ shaderProgram [aVertexPosition, aVertexColor] [uPMatrix,uMVMatrix] -> do
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
          clearColor 0.0 0.0 0.0 1.0
          enable _DEPTH_TEST
          let state = {
                        context : context,
                        shaderProgram : shaderProgram,
                        aVertexPosition : aVertexPosition,
                        aVertexColor : aVertexColor,
                        uPMatrix : uPMatrix,
                        uMVMatrix : uMVMatrix,
                        buf1 : buf1,
                        buf1Colors : buf1Colors,
                        buf2 : buf2,
                        buf2Colors : buf2Colors,
                        lastTime : Nothing,
                        rTri : 0,
                        rSquare : 0
                      }
          tick state

tick :: forall eff. State (trace :: Trace, now :: Now |eff)  ->  EffWebGL (trace :: Trace, now :: Now |eff) Unit
tick state = do
--  trace ("tick: " ++ show state.lastTime)
  drawScene state
  state' <- animate state
  return unit
  requestAnimationFrame (tick state')

animate ::  forall eff. State (now :: Now | eff) -> EffWebGL (now :: Now |eff) (State (now :: Now |eff))
animate state = do
  timeNow <- liftM1 toEpochMilliseconds now
  case state.lastTime of
    Nothing -> return state {lastTime = Just timeNow}
    Just lastt ->
      let elapsed = timeNow - lastt
      in return state {lastTime = Just timeNow,
                       rTri = state.rTri + (90 * elapsed) / 1000.0,
                       rSquare = state.rSquare + (75 * elapsed) / 1000.0}

drawScene :: forall eff. State (now :: Now |eff)  -> EffWebGL (now :: Now |eff) Unit
drawScene state = do
      canvasWidth <- state.context.getCanvasWidth
      canvasHeight <- state.context.getCanvasHeight
      viewport 0 0 canvasWidth canvasHeight
      clear (_COLOR_BUFFER_BIT .|. _DEPTH_BUFFER_BIT)

      let pMatrix = M4.makePerspective 45 (canvasWidth / canvasHeight) 0.1 100.0
      setMatrix state.shaderProgram state.uPMatrix pMatrix
      let mvMatrix =
          M4.rotate (degToRad state.rTri) (V3.vec3' [0, 1, 0])
            $ M4.translate  (V3.vec3 (-1.5) 0.0 (-7.0)) M4.identity

      setMatrix state.shaderProgram state.uMVMatrix mvMatrix

      bindBuffer _ARRAY_BUFFER state.buf1Colors
      vertexPointer state.shaderProgram state.aVertexColor
      drawBuffer state.shaderProgram state.buf1 state.aVertexPosition _TRIANGLES 3

      let mvMatrix =
          M4.rotate (degToRad state.rSquare) (V3.vec3' [1, 0, 0])
            $ M4.translate  (V3.vec3 (1.5) 0.0 (-7.0)) M4.identity
      setMatrix state.shaderProgram state.uMVMatrix mvMatrix

      bindBuffer _ARRAY_BUFFER state.buf2Colors
      vertexPointer state.shaderProgram state.aVertexColor
      drawBuffer state.shaderProgram state.buf2 state.aVertexPosition _TRIANGLE_STRIP 4

-- | Convert from radians to degrees.
radToDeg :: Number -> Number
radToDeg x = x/pi*180

-- | Convert from degrees to radians.
degToRad :: Number -> Number
degToRad x = x/180*pi
