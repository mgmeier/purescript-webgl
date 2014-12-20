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

main :: Eff (trace :: Trace, alert :: Alert) Unit
main = runWebGL "glcanvas" (\s -> alert s) $ do
  trace "WebGL started"
  withShaders fshaderSource vshaderSource (\s -> alert s)
    \ shaderProgram -> do

      clearColor 0.0 0.0 0.0 1.0
      enable _DEPTH_TEST

      triangleVertexPositionBuffer <- createBuffer
      bindBuffer _ARRAY_BUFFER triangleVertexPositionBuffer
      let vertices = [0.0,  1.0,  0.0,
                    (-1.0), (-1.0),  0.0,
                      1.0, (-1.0),  0.0]
      let arrayBuffer = asFloat32Array vertices
      bufferData _ARRAY_BUFFER arrayBuffer _STATIC_DRAW

{-
      squareVertexPositionBuffer <- createBuffer
      bindBuffer _ARRAY_BUFFER squareVertexPositionBuffer
      let vertices = [
             1.0,  1.0,  0.0,
            (-1.0),  1.0,  0.0,
             1.0, (-1.0),  0.0,
            (-1.0), (-1.0),  0.0]
      let arrayBuffer  = asArrayBuffer vertices
      bufferData _ARRAY_BUFFER arrayBuffer _STATIC_DRAW
-}

      clear (_COLOR_BUFFER_BIT .|. _DEPTH_BUFFER_BIT)
      let width = canvasWidth "glcanvas"
          height = canvasHeight "glcanvas"
      viewport 0 0 width height

--        mat4.perspective(45, gl.viewportWidth / gl.viewportHeight, 0.1, 100.0, pMatrix);
--        mat4.identity(mvMatrix);
--        mat4.translate(mvMatrix, [-1.5, 0.0, -7.0]);
      bindBuffer _ARRAY_BUFFER triangleVertexPositionBuffer
      vpa <- getAttribLocation shaderProgram "aVertexPosition"
      vertexAttribPointer vpa 3 _FLOAT false 0 0
--      pMatrixUniform <- getUniformLocation shaderProgram "uPMatrix"
      drawArrays _TRIANGLES 0 3
      trace "WebGL completed"
      return unit

{-
      uniformMatrix4fv (shaderProgram.pMatrixUniform, false, pMatrix);

        setMatrixUniforms();
        gl.drawArrays(gl.TRIANGLES, 0, triangleVertexPositionBuffer.numItems);


        mat4.translate(mvMatrix, [3.0, 0.0, 0.0]);
        gl.bindBuffer(gl.ARRAY_BUFFER, squareVertexPositionBuffer);
        gl.vertexAttribPointer(shaderProgram.vertexPositionAttribute, squareVertexPositionBuffer.itemSize, gl.FLOAT, false, 0, 0);
        setMatrixUniforms();
        gl.drawArrays(gl.TRIANGLE_STRIP, 0, squareVertexPositionBuffer.numItems);

      clear (_COLOR_BUFFER_BIT .|. _DEPTH_BUFFER_BIT)
      perspect
      return unit
-}
{-

        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
        triangleVertexPositionBuffer.itemSize = 3;
        triangleVertexPositionBuffer.numItems = 3;

        squareVertexPositionBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, squareVertexPositionBuffer);
        vertices = [
             1.0,  1.0,  0.0,
            -1.0,  1.0,  0.0,
             1.0, -1.0,  0.0,
            -1.0, -1.0,  0.0
        ];
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
        squareVertexPositionBuffer.itemSize = 3;
        squareVertexPositionBuffer.numItems = 4;
    }
-}
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

--
