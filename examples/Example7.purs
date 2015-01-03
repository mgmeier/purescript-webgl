-- needs todays compiler fixes, to allow underscores in constructor names.
-- need to start chrome with --allow-file-access-from-files to be able to load local files
-- Example 7: Lightning (Open with index7.html)
module Main where

import Control.Monad.Eff.WebGL
import Graphics.WebGL
import Graphics.WebGLRaw
import Graphics.WebGLTexture
import qualified Data.Matrix as M
import qualified Data.Matrix4 as M
import qualified Data.Matrix3 as M
import qualified Data.VecMat as M
import qualified Data.Vector as V
import qualified Data.Vector3 as V
import qualified Data.TypedArray as T
import Control.Monad.Eff.Alert

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

fshaderSource :: String
fshaderSource =
"""
    precision mediump float;

    varying vec2 vTextureCoord;
    varying vec3 vLightWeighting;

    uniform sampler2D uSampler;

    void main(void) {
        vec4 textureColor = texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t));
        gl_FragColor = vec4(textureColor.rgb * vLightWeighting, textureColor.a);
    }
"""

vshaderSource :: String
vshaderSource =
"""
    attribute vec3 aVertexPosition;
    attribute vec3 aVertexNormal;
    attribute vec2 aTextureCoord;

    uniform mat4 uMVMatrix;
    uniform mat4 uPMatrix;
    uniform mat3 uNMatrix;

    uniform vec3 uAmbientColor;

    uniform vec3 uLightingDirection;
    uniform vec3 uDirectionalColor;

    uniform bool uUseLighting;

    varying vec2 vTextureCoord;
    varying vec3 vLightWeighting;

    void main(void) {
        gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
        vTextureCoord = aTextureCoord;

        if (!uUseLighting) {
            vLightWeighting = vec3(1.0, 1.0, 1.0);
        } else {
            vec3 transformedNormal = uNMatrix * aVertexNormal;
            float directionalLightWeighting = max(dot(transformedNormal, uLightingDirection), 0.0);
            vLightWeighting = uAmbientColor + uDirectionalColor * directionalLightWeighting;
        }
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

vertexNormals = [
        -- Front face
         0.0,  0.0,  1.0,
         0.0,  0.0,  1.0,
         0.0,  0.0,  1.0,
         0.0,  0.0,  1.0,

        -- Back face
         0.0,  0.0, -1.0,
         0.0,  0.0, -1.0,
         0.0,  0.0, -1.0,
         0.0,  0.0, -1.0,

        -- Top face
         0.0,  1.0,  0.0,
         0.0,  1.0,  0.0,
         0.0,  1.0,  0.0,
         0.0,  1.0,  0.0,

        -- Bottom face
         0.0, -1.0,  0.0,
         0.0, -1.0,  0.0,
         0.0, -1.0,  0.0,
         0.0, -1.0,  0.0,

        -- Right face
         1.0,  0.0,  0.0,
         1.0,  0.0,  0.0,
         1.0,  0.0,  0.0,
         1.0,  0.0,  0.0,

        -- Left face
        -1.0,  0.0,  0.0,
        -1.0,  0.0,  0.0,
        -1.0,  0.0,  0.0,
        -1.0,  0.0,  0.0
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

                aVertexPosition :: AttrLocation,
                aVertexNormal :: AttrLocation,
                aTextureCoord :: AttrLocation,
                uPMatrix :: UniLocation,
                uMVMatrix :: UniLocation,
                uNMatrix :: UniLocation,
                uSampler :: UniLocation,
                uUseLighting :: UniLocation,
                uAmbientColor :: UniLocation,
                uLightingDirection :: UniLocation,
                uDirectionalColor :: UniLocation,

                cubeVertices :: Buffer T.Float32,
                cubeVerticesNormal :: Buffer T.Float32,
                textureCoords :: Buffer T.Float32,
                cubeVertexIndices :: Buffer T.Uint16,
                texture :: WebGLTex,

                lastTime :: Maybe Number,
                xRot :: Number,
                xSpeed :: Number,
                yRot :: Number,
                ySpeed :: Number,
                z :: Number,
                currentlyPressedKeys :: [Number]
              }

main :: Eff (trace :: Trace, alert :: Alert, now :: Now) Unit
main = do
  runWebGL
    "glcanvas"
    (\s -> alert s)
      \ context -> do
        trace "WebGL started"
        withShaders fshaderSource
                    vshaderSource
                    [VecAttr Three "aVertexPosition", VecAttr Three "aVertexNormal", VecAttr Two "aTextureCoord"]
                    [Matrix Four "uPMatrix", Matrix Four "uMVMatrix", Matrix Three "uNMatrix", Sampler2D "uSampler",
                     Bool One "uUseLighting", Vec Three "uAmbientColor", Vec Three "uLightingDirection",
                     Vec Three "uDirectionalColor"]
                    (\s -> alert s)
                      \ shaderProgram [aVertexPosition, aVertexNormal,
                                      aTextureCoord]
                          [uPMatrix,uMVMatrix,
                          uNMatrix,uSampler,
                          uUseLighting,uAmbientColor,
                          uLightingDirection,uDirectionalColor]
                            -> do
          cubeVertices <- makeBufferSimple cubeV
          cubeVerticesNormal <- makeBufferSimple vertexNormals

          textureCoords <- makeBufferSimple texCoo
          cubeVertexIndices <- makeBuffer ELEMENT_ARRAY_BUFFER T.asUint16Array cvi
          clearColor 0.0 0.0 0.0 1.0
          enable DEPTH_TEST
          texture2DFor "crate.gif" MIPMAP \texture -> do
            let state = {
                          context : context,
                          shaderProgram : shaderProgram,

                          aVertexPosition : aVertexPosition,
                          aVertexNormal : aVertexNormal,
                          aTextureCoord : aTextureCoord,
                          uPMatrix : uPMatrix,
                          uMVMatrix : uMVMatrix,
                          uNMatrix : uNMatrix,
                          uSampler : uSampler,
                          uUseLighting : uUseLighting,
                          uAmbientColor : uAmbientColor,
                          uLightingDirection : uLightingDirection,
                          uDirectionalColor : uDirectionalColor,
                          cubeVertices : cubeVertices,
                          cubeVerticesNormal : cubeVerticesNormal,
                          textureCoords : textureCoords,
                          cubeVertexIndices : cubeVertexIndices,
                          texture : texture,
                          lastTime : (Nothing :: Maybe Number),

                          xRot : 0,
                          xSpeed : 1.0,
                          yRot : 0,
                          ySpeed : 1.0,
                          z : (-5.0),
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
  setMatrix4 s.uPMatrix pMatrix

  let mvMatrix =
      M.rotate (degToRad s.yRot) (V.vec3' [0, 1, 0])
        $ M.rotate (degToRad s.xRot) (V.vec3' [1, 0, 0])
          $ M.translate  (V.vec3 0.0 0.0 s.z)
            $ M.identity
  setMatrix4 s.uMVMatrix mvMatrix

  let nMatrix = M.transpose
                  $ fromJust
                    $ M.normalFromMat4 mvMatrix
  setMatrix3 s.uNMatrix nMatrix

  bindPointBuf s.cubeVertices s.aVertexPosition
  bindPointBuf s.cubeVerticesNormal s.aVertexNormal
  bindPointBuf s.textureCoords s.aTextureCoord

  withTexture2D s.texture 0 s.uSampler 0

  setLightning s

  bindBuf s.cubeVertexIndices
  drawElements TRIANGLES s.cubeVertexIndices.bufferSize

setLightning :: forall eff. State -> EffWebGL eff Unit
setLightning s = do
  lighting <- getElementByIdBool "lighting"
  setBool1 s.uUseLighting lighting
  if lighting
    then do
      ar <- getElementByIdFloat "ambientR"
      ag <- getElementByIdFloat "ambientG"
      ab <- getElementByIdFloat "ambientB"
      setVector3 s.uAmbientColor (V.Vec [ar, ag, ab])
      lx <- getElementByIdFloat "lightDirectionX"
      ly <- getElementByIdFloat "lightDirectionY"
      lz <- getElementByIdFloat "lightDirectionZ"
      let v = V.scale (-1)
                  $ V.normalize
                    $ V.vec3 lx ly lz
      setVector3 s.uLightingDirection v
      dr <- getElementByIdFloat "directionalR"
      dg <- getElementByIdFloat "directionalG"
      db <- getElementByIdFloat "directionalB"
      setVector3 s.uDirectionalColor (V.Vec [dr, dg, db])
    else return unit


foreign import getElementByIdFloat
"""
  function getElementByIdFloat(targ_id) {
      return function () {
        return parseFloat(document.getElementById(targ_id).value);
      };
    }
""" :: forall eff. String -> (EffWebGL eff Number)

foreign import getElementByIdBool
"""
  function getElementByIdBool(targ_id) {
      return function () {
        return document.getElementById(targ_id).checked;
      };
    }
""" :: forall eff. String -> (EffWebGL eff Boolean)

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
  let cp = if elemIndex key s.currentlyPressedKeys /= -1
              then s.currentlyPressedKeys
              else key : s.currentlyPressedKeys
  writeSTRef stRef (s {currentlyPressedKeys = cp})
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
--      trace (show s.currentlyPressedKeys)
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
