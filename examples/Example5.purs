-- needs todays compiler fixes, to allow underscores in constructor names.
-- need to start chrome with --allow-file-access-from-files to be able to load local files
-- need line 7?:   let stupid = Mat3 "stupid", cause otherwise Mat3 is unknown
-- or need to add Graphics.WebGL as options/modules to gruntfile.
module Main where

import Control.Monad.Eff.WebGL
import Graphics.WebGL
import Graphics.WebGLTexture
import qualified Data.Matrix4 as M
import qualified Data.Matrix as M
import qualified Data.Vector3 as V3
import Control.Monad.Eff.Alert
import qualified Data.TypedArray as T

import Control.Monad.Eff
import Control.Monad
import Debug.Trace
import Data.Tuple
import Data.Date
import Data.Maybe
import Data.Array
import Math

shaders :: Shaders {aVertexPosition :: Attribute Vec3, aTextureCoord :: Attribute Vec2,
                      uPMatrix :: Uniform Mat4, uMVMatrix:: Uniform Mat4, uSampler :: Uniform Sampler2D}
shaders = Shaders

  """ precision mediump float;

      varying vec2 vTextureCoord;

      uniform sampler2D uSampler;

      void main(void) {
          gl_FragColor = texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t));
      }
  """

  """
      attribute vec3 aVertexPosition;
      attribute vec2 aTextureCoord;

      uniform mat4 uMVMatrix;
      uniform mat4 uPMatrix;

      varying vec2 vTextureCoord;


      void main(void) {
          gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
          vTextureCoord = aTextureCoord;
      }
  """

type State =
    {
        context :: WebGLContext,
        shaderProgram :: WebGLProg,

        aVertexPosition :: Attribute Vec3,
        aTextureCoord :: Attribute Vec2,
        uPMatrix :: Uniform Mat4,
        uMVMatrix :: Uniform Mat4,
        uSampler :: Uniform Sampler2D,

        cubeVertices :: Buffer T.Float32,
        textureCoords :: Buffer T.Float32,
        cubeVertexIndices :: Buffer T.Uint16,
        texture :: WebGLTex,

        lastTime :: Maybe Number,
        rot :: Number
    }

main :: Eff (trace :: Trace, alert :: Alert, now :: Now) Unit
main = do
--  let stupid = Mat3 "stupid"
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        trace "WebGL started"
        withShaders shaders
                    (\s -> alert s)
                      \ binding -> do
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

          textureCoords <- makeBufferSimple [
                            -- Front face
                            0.0, 0.0,
                            1.0, 0.0,
                            1.0, 1.0,
                            0.0, 1.0,

                            -- Back face
                            1.0, 0.0,
                            1.0, 1.0,
                            0.0, 1.0,
                            0.0, 0.0,

                            -- Top face
                            0.0, 1.0,
                            0.0, 0.0,
                            1.0, 0.0,
                            1.0, 1.0,

                            -- Bottom face
                            1.0, 1.0,
                            0.0, 1.0,
                            0.0, 0.0,
                            1.0, 0.0,

                            -- Right face
                            1.0, 0.0,
                            1.0, 1.0,
                            0.0, 1.0,
                            0.0, 0.0,

                            -- Left face
                            0.0, 0.0,
                            1.0, 0.0,
                            1.0, 1.0,
                            0.0, 1.0
                          ]
          cubeVertexIndices <- makeBuffer ELEMENT_ARRAY_BUFFER T.asUint16Array
                          [
                            0, 1, 2,      0, 2, 3,    -- Front face
                            4, 5, 6,      4, 6, 7,    -- Back face
                            8, 9, 10,     8, 10, 11,  -- Top face
                            12, 13, 14,   12, 14, 15, -- Bottom face
                            16, 17, 18,   16, 18, 19, -- Right face
                            20, 21, 22,   20, 22, 23  -- Left face
                          ]
          clearColor 0.0 0.0 0.0 1.0
          enable DEPTH_TEST
          texture2DFor "test.png" MIPMAP \texture ->
            tick {
                  context : context,
                  shaderProgram : binding.webGLProgram,

                  aVertexPosition : binding.aVertexPosition,
                  aTextureCoord : binding.aTextureCoord,
                  uPMatrix : binding.uPMatrix,
                  uMVMatrix : binding.uMVMatrix,
                  uSampler : binding.uSampler,

                  cubeVertices : cubeVertices,
                  textureCoords : textureCoords,
                  cubeVertexIndices : cubeVertexIndices,
                  texture : texture,
                  lastTime : Nothing,
                  rot : 0
                }


tick :: forall eff. State  ->  EffWebGL (trace :: Trace, now :: Now |eff) Unit
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
                       rot = state.rot + (90 * elapsed) / 1000.0
                       }

drawScene :: forall eff. State -> EffWebGL (now :: Now |eff) Unit
drawScene s = do
      canvasWidth <- getCanvasWidth s.context
      canvasHeight <- getCanvasHeight s.context
      viewport 0 0 canvasWidth canvasHeight
      clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

      let pMatrix = M.makePerspective 45 (canvasWidth / canvasHeight) 0.1 100.0
      setUniformFloats s.uPMatrix (M.toArray pMatrix)

      let mvMatrix = M.rotate (degToRad s.rot) (V3.vec3' [1, 0, 0])
                        $ M.rotate (degToRad s.rot) (V3.vec3' [0, 1, 0])
                          $ M.rotate (degToRad s.rot) (V3.vec3' [0, 0, 1])
                            $ M.translate  (V3.vec3 0.0 0.0 (-8.0))
                              $ M.identity

      setUniformFloats s.uMVMatrix (M.toArray mvMatrix)

      bindPointBuf s.cubeVertices s.aVertexPosition
      bindPointBuf s.textureCoords s.aTextureCoord

      withTexture2D s.texture 0 s.uSampler 0

      bindBuf s.cubeVertexIndices
      drawElements TRIANGLES s.cubeVertexIndices.bufferSize

-- | Convert from radians to degrees.
radToDeg :: Number -> Number
radToDeg x = x/pi*180

-- | Convert from degrees to radians.
degToRad :: Number -> Number
degToRad x = x/180*pi
