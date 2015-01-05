module Main where

import Control.Monad.Eff.WebGL
import Graphics.WebGL
import qualified Data.Matrix4 as M
import qualified Data.Matrix as M
import qualified Data.Vector3 as V3
import Control.Monad.Eff.Alert
import qualified Data.TypedArray as T

import Control.Monad.Eff
import Control.Monad
import Debug.Trace
import Data.Date
import Data.Maybe
import Data.Array
import Math

shaders :: Shaders {aVertexPosition :: Attribute Vec3, aVertexColor :: Attribute Vec3,
                      uPMatrix :: Uniform Mat4, uMVMatrix:: Uniform Mat4}
shaders = Shaders

  """precision mediump float;

  varying vec4 vColor;

  void main(void) {
    gl_FragColor = vColor;
      }
  """

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

type State = {
                context :: WebGLContext,
                shaderProgram :: WebGLProg,
                aVertexPosition :: Attribute Vec3,
                aVertexColor  :: Attribute Vec3,
                uPMatrix :: Uniform Mat4,
                uMVMatrix :: Uniform Mat4,
                pyramidVertices ::Buffer T.Float32,
                pyramidColors :: Buffer T.Float32,
                cubeVertices :: Buffer T.Float32,
                cubeColors :: Buffer T.Float32,
                cubeVertexIndices :: Buffer T.Uint16,
                lastTime :: Maybe Number,
                rPyramid :: Number,
                rCube :: Number
              }

main :: Eff (trace :: Trace, alert :: Alert, now :: Now) Unit
main =
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        trace "WebGL started"
        withShaders shaders
                    (\s -> alert s)
                      \ bindings -> do
          pyramidVertices <- makeBufferSimple [
                              -- Front face
                               0.0,  1.0,  0.0,
                              -1.0, -1.0,  1.0,
                               1.0, -1.0,  1.0,

                              -- Right face
                               0.0,  1.0,  0.0,
                               1.0, -1.0,  1.0,
                               1.0, -1.0, -1.0,

                              -- Back face
                               0.0,  1.0,  0.0,
                               1.0, -1.0, -1.0,
                              -1.0, -1.0, -1.0,

                              -- Left face
                               0.0,  1.0,  0.0,
                              -1.0, -1.0, -1.0,
                              -1.0, -1.0,  1.0]
          pyramidColors <- makeBufferSimple   [
                              -- Front face
                              1.0, 0.0, 0.0, 1.0,
                              0.0, 1.0, 0.0, 1.0,
                              0.0, 0.0, 1.0, 1.0,

                              -- Right face
                              1.0, 0.0, 0.0, 1.0,
                              0.0, 0.0, 1.0, 1.0,
                              0.0, 1.0, 0.0, 1.0,

                              -- Back face
                              1.0, 0.0, 0.0, 1.0,
                              0.0, 1.0, 0.0, 1.0,
                              0.0, 0.0, 1.0, 1.0,

                              -- Left face
                              1.0, 0.0, 0.0, 1.0,
                              0.0, 0.0, 1.0, 1.0,
                              0.0, 1.0, 0.0, 1.0]
          cubeVertices <- makeBufferSimple [
                            -- Front face
                            -1.0, -1.0,  1.0,
                             1.0, -1.0,  1.0,
                             1.0,  1.0,  1.0,
                            -1.0,  1.0,  1.0,

                            -- Back face
                            -1.0, -1.0, -1.0,
                            -1.0,  1.0, -1.0,
                             1.0,  1.0, -1.0,
                             1.0, -1.0, -1.0,

                            -- Top face
                            -1.0,  1.0, -1.0,
                            -1.0,  1.0,  1.0,
                             1.0,  1.0,  1.0,
                             1.0,  1.0, -1.0,

                            -- Bottom face
                            -1.0, -1.0, -1.0,
                             1.0, -1.0, -1.0,
                             1.0, -1.0,  1.0,
                            -1.0, -1.0,  1.0,

                            -- Right face
                             1.0, -1.0, -1.0,
                             1.0,  1.0, -1.0,
                             1.0,  1.0,  1.0,
                             1.0, -1.0,  1.0,

                            -- Left face
                            -1.0, -1.0, -1.0,
                            -1.0, -1.0,  1.0,
                            -1.0,  1.0,  1.0,
                            -1.0,  1.0, -1.0]
          cubeColors <- makeBufferSimple $ concat $ concatMap (\e -> [e,e,e,e])
                              [[1.0, 0.0, 0.0, 1.0], -- Front face
                              [1.0, 1.0, 0.0, 1.0], -- Back face
                              [0.0, 1.0, 0.0, 1.0], -- Top face
                              [1.0, 0.5, 0.5, 1.0], -- Bottom face
                              [1.0, 0.0, 1.0, 1.0], -- Right face
                              [0.0, 0.0, 1.0, 1.0]]  -- Left face
          cubeVertexIndices <- makeBuffer ELEMENT_ARRAY_BUFFER T.asUint16Array [
                              0, 1, 2,      0, 2, 3,    -- Front face
                              4, 5, 6,      4, 6, 7,    -- Back face
                              8, 9, 10,     8, 10, 11,  -- Top face
                              12, 13, 14,   12, 14, 15, -- Bottom face
                              16, 17, 18,   16, 18, 19, -- Right face
                              20, 21, 22,   20, 22, 23]  -- Left face]
          clearColor 0.0 0.0 0.0 1.0
          enable DEPTH_TEST
          let state = {
                        context : context,
                        shaderProgram : bindings.webGLProgram,
                        aVertexPosition : bindings.aVertexPosition,
                        aVertexColor : bindings.aVertexColor,
                        uPMatrix : bindings.uPMatrix,
                        uMVMatrix : bindings.uMVMatrix,
                        pyramidVertices : pyramidVertices,
                        pyramidColors : pyramidColors,
                        cubeVertices : cubeVertices,
                        cubeColors : cubeColors,
                        cubeVertexIndices : cubeVertexIndices,
                        lastTime : Nothing,
                        rPyramid : 0,
                        rCube : 0
                      }
          tick state

tick :: forall eff. State ->  EffWebGL (trace :: Trace, now :: Now |eff) Unit
tick state = do
--  trace ("tick: " ++ show state.lastTime)
  drawScene state
  state' <- animate state
  requestAnimationFrame (tick state')

animate ::  forall eff. State -> EffWebGL (now :: Now |eff) State
animate state = do
  timeNow <- liftM1 toEpochMilliseconds now
  case state.lastTime of
    Nothing -> return state {lastTime = Just timeNow}
    Just lastt ->
      let elapsed = timeNow - lastt
      in return state {lastTime = Just timeNow,
                       rPyramid = state.rPyramid + (90 * elapsed) / 1000.0,
                       rCube = state.rCube + (75 * elapsed) / 1000.0}

drawScene :: forall eff. State -> EffWebGL (now :: Now |eff) Unit
drawScene s = do
      canvasWidth <- getCanvasWidth s.context
      canvasHeight <- getCanvasHeight s.context
      viewport 0 0 canvasWidth canvasHeight
      clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

-- The pyramid
      let pMatrix = M.makePerspective 45 (canvasWidth / canvasHeight) 0.1 100.0
      setUniformFloats s.uPMatrix (M.toArray pMatrix)
      let mvMatrix =
          M.rotate (degToRad s.rPyramid) (V3.vec3' [0, 1, 0])
            $ M.translate  (V3.vec3 (-1.5) 0.0 (-8.0))
              $ M.identity

      setUniformFloats s.uMVMatrix (M.toArray mvMatrix)
      bindPointBuf s.pyramidColors s.aVertexColor
      drawArr TRIANGLES s.pyramidVertices s.aVertexPosition

-- The cube
      let mvMatrix =
          M.rotate (degToRad s.rCube) (V3.vec3' [1, 1, 1])
            $ M.translate  (V3.vec3 (1.5) 0.0 (-8.0))
              $ M.identity
      setUniformFloats s.uMVMatrix (M.toArray mvMatrix)

      bindPointBuf s.cubeColors s.aVertexColor
      bindPointBuf s.cubeVertices s.aVertexPosition
      bindBuf s.cubeVertexIndices
      drawElements TRIANGLES s.cubeVertexIndices.bufferSize


-- | Convert from radians to degrees.
radToDeg :: Number -> Number
radToDeg x = x/pi*180

-- | Convert from degrees to radians.
degToRad :: Number -> Number
degToRad x = x/180*pi
