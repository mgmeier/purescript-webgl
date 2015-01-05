-- needs todays compiler fixes, to allow underscores in constructor names.
-- need to start chrome with --allow-file-access-from-files to be able to load local files
-- Example 6: Keyboard, different tetures, ...
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
import Control.Monad.ST
import Debug.Trace
import Data.Tuple
import Data.Date
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Array
import Math

shaders :: Shaders {aVertexPosition :: Attribute Vec3, aTextureCoord :: Attribute Vec2,
                      uPMatrix :: Uniform Mat4, uMVMatrix:: Uniform Mat4, uSampler :: Uniform Sampler2D}
shaders = Shaders
  """
      precision mediump float;

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

cubeV = [
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
        -1.0,  1.0, -1.0
      ]

texCoo = [
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

cvi = [
        0, 1, 2,      0, 2, 3,    -- Front face
        4, 5, 6,      4, 6, 7,    -- Back face
        8, 9, 10,     8, 10, 11,  -- Top face
        12, 13, 14,   12, 14, 15, -- Bottom face
        16, 17, 18,   16, 18, 19, -- Right face
        20, 21, 22,   20, 22, 23  -- Left face
      ]

type State = {
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
                textures :: [WebGLTex],

                lastTime :: Maybe Number,
                xRot :: Number,
                xSpeed :: Number,
                yRot :: Number,
                ySpeed :: Number,
                z :: Number,
                filterInd :: Number,
                currentlyPressedKeys :: [Number]
            }

main :: Eff (trace :: Trace, alert :: Alert, now :: Now) Unit
main = do
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        trace "WebGL started"
        withShaders shaders
                    (\s -> alert s)
                      \ bindings -> do
          cubeVertices <- makeBufferSimple cubeV
          textureCoords <- makeBufferSimple texCoo
          cubeVertexIndices <- makeBuffer ELEMENT_ARRAY_BUFFER T.asUint16Array cvi
          clearColor 0.0 0.0 0.0 1.0
          enable DEPTH_TEST
          texture2DFor "crate.gif" NEAREST \texture1 ->
            texture2DFor "crate.gif" LINEAR \texture2 ->
              texture2DFor "crate.gif" MIPMAP \texture3 -> do
                let state = {
                              context : context,
                              shaderProgram : bindings.webGLProgram,

                              aVertexPosition : bindings.aVertexPosition,
                              aTextureCoord : bindings.aTextureCoord,
                              uPMatrix : bindings.uPMatrix,
                              uMVMatrix : bindings.uMVMatrix,
                              uSampler : bindings.uSampler,

                              cubeVertices : cubeVertices,
                              textureCoords : textureCoords,
                              cubeVertexIndices : cubeVertexIndices,
                              textures : [texture1,texture2,texture3],
                              lastTime : (Nothing :: Maybe Number),

                              xRot : 0,
                              xSpeed : 1.0,
                              yRot : 0,
                              ySpeed : 1.0,
                              z : (-5.0),
                              filterInd : 0,
                              currentlyPressedKeys : []
                            }
                runST do
                  stRef <- newSTRef state
                  onKeyDown (handleKeyD stRef)
                  onKeyUp (handleKeyU stRef)
                  tick stRef

tick :: forall h eff. STRef h State ->  EffWebGL (st :: ST h, trace :: Trace, now :: Now |eff) Unit
tick stRef = do
  drawScene stRef
  handleKeys stRef
  animate stRef
  requestAnimationFrame (tick stRef)

animate ::  forall h eff . STRef h State -> EffWebGL (st :: ST h, now :: Now |eff) Unit
animate stRef = do
  s <- readSTRef stRef
  timeNow <- liftM1 toEpochMilliseconds now
  case s.lastTime of
    Nothing -> writeSTRef stRef (s {lastTime = Just timeNow})
    Just lastt ->
      let elapsed = timeNow - lastt
      in writeSTRef stRef (s {lastTime = Just timeNow,
                              xRot = s.xRot + s.xSpeed * elapsed / 1000.0,
                              yRot = s.yRot + s.ySpeed * elapsed / 1000.0
                              })
  return unit

drawScene :: forall h eff . STRef h State -> EffWebGL (st :: ST h |eff) Unit
drawScene stRef = do
  s <- readSTRef stRef
  canvasWidth <- getCanvasWidth s.context
  canvasHeight <- getCanvasHeight s.context
  viewport 0 0 canvasWidth canvasHeight
  clear [COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT]

  let pMatrix = M.makePerspective 45 (canvasWidth / canvasHeight) 0.1 100.0
  setUniformFloats s.uPMatrix (M.toArray pMatrix)

  let mvMatrix =
      M.rotate (degToRad s.xRot) (V3.vec3' [1, 0, 0])
        $ M.rotate (degToRad s.yRot) (V3.vec3' [0, 1, 0])
          $ M.translate (V3.vec3 0.0 0.0 s.z)
            $ M.identity
  setUniformFloats s.uMVMatrix (M.toArray mvMatrix)

  bindPointBuf s.cubeVertices s.aVertexPosition
  bindPointBuf s.textureCoords s.aTextureCoord

  withTexture2D (fromJust $ s.textures !! s.filterInd) 0 s.uSampler 0

  bindBuf s.cubeVertexIndices
  drawElements TRIANGLES s.cubeVertexIndices.bufferSize



-- | Convert from radians to degrees.
radToDeg :: Number -> Number
radToDeg x = x/pi*180

-- | Convert from degrees to radians.
degToRad :: Number -> Number
degToRad x = x/180*pi

-- * Key handling

handleKeys ::  forall h eff . STRef h State -> EffWebGL (trace :: Trace, st :: ST h |eff) Unit
handleKeys stRef = do
  s <- readSTRef stRef
  if null s.currentlyPressedKeys
    then return unit
    else
      let z' = if elemIndex 33 s.currentlyPressedKeys /= -1
                  then s.z - 0.05
                  else s.z
          z'' = if elemIndex 34 s.currentlyPressedKeys /= -1
                  then z' + 0.05
                  else z'
          ySpeed' = if elemIndex 37 s.currentlyPressedKeys /= -1
                  then s.ySpeed - 1
                  else s.ySpeed
          ySpeed'' = if elemIndex 39 s.currentlyPressedKeys /= -1
                  then ySpeed' + 1
                  else ySpeed'
          xSpeed' = if elemIndex 38 s.currentlyPressedKeys /= -1
                  then s.xSpeed - 1
                  else s.xSpeed
          xSpeed'' = if elemIndex 40 s.currentlyPressedKeys /= -1
                  then xSpeed' + 1
                  else xSpeed'
      in do
        writeSTRef stRef (s{z=z'',ySpeed=ySpeed'',xSpeed=xSpeed''})
        trace (show s.currentlyPressedKeys)
        return unit

handleKeyD :: forall h eff. STRef h State -> Event -> Eff (st :: ST h, trace :: Trace | eff) Unit
handleKeyD stRef event = do
  trace "handleKeyDown"
  let key = eventGetKeyCode event
  s <- readSTRef stRef
  let f = if key == 70
            then if s.filterInd + 1 == 3
                    then 0
                    else s.filterInd + 1
            else s.filterInd
      cp = if elemIndex key s.currentlyPressedKeys /= -1
              then s.currentlyPressedKeys
              else key : s.currentlyPressedKeys
  trace ("filterInd: " ++ show f)
  writeSTRef stRef (s {currentlyPressedKeys = cp, filterInd = f})
--  trace (show s.currentlyPressedKeys)
  return unit

handleKeyU :: forall h eff. STRef h State -> Event -> Eff (st :: ST h, trace :: Trace | eff) Unit
handleKeyU stRef event = do
  trace "handleKeyUp"
  let key = eventGetKeyCode event
  s <- readSTRef stRef
  if elemIndex key s.currentlyPressedKeys == -1
    then return unit
    else do
      writeSTRef stRef (s {currentlyPressedKeys = delete key s.currentlyPressedKeys})
      trace (show s.currentlyPressedKeys)
      return unit

foreign import data Event :: *

foreign import onKeyDown
"""
        function onKeyDown(handleKeyDown) {
          return function() {
            document.onkeydown = function(event) {handleKeyDown(event)()};
            };}
""" ::  forall eff. (Event -> Eff (webgl :: WebGl | eff) Unit)
    -> Eff (webgl :: WebGl | eff) Unit

foreign import onKeyUp
"""
        function onKeyUp(handleKeyUp) {
          return function() {
            document.onkeyup = function(event) {handleKeyUp(event)()};
            };}
""" ::  forall eff. (Event -> Eff (webgl :: WebGl | eff) Unit)
    -> Eff (webgl :: WebGl | eff) Unit

foreign import eventGetKeyCode
"""
  function eventGetKeyCode (event) {
      return (event.keyCode);
      }
""" :: Event -> Number
