[![Build Status](https://travis-ci.org/jutaro/purescript-webgl.svg?branch=master)](https://travis-ci.org/jutaro/purescript-webgl)

Binding to webGL for purescript.
 
- [Module documentation](docs/Module.md)


This modules should be imported like: 
~~~
import Control.Monad.Eff.WebGL
import Graphics.WebGL
import Graphics.WebGLTexture
~~~
 
or 
~~~
import qualified Control.Monad.Eff.WebGL as GL
import qualified Graphics.WebGL as GL
import qualified Graphics.WebGLTexture as GL
~~~


I started with generating the raw interface by parsing the Kronos IDL and generating
a low level binding to Graphics.WebGLRaw. This can be found in the package 
[purescript-webgl-generator](https://github.com/jutaro/purescript-webgl-generator). 
For some procedures the generation is not appropriate (e.g. ad-hoc polymorphism 
in Javascript or calling the same procedure 
with different number of arguments). I decided to copy these cases by hand and modify them,
instead of making a super smart generator. All the constants are generated with a leading
underscore, while all the procedures have a trailing underscore in this module. I 
insist on that this module should not be changed by hand. I suggest you don't call 
WebGLRaw directly, but expand the binding and provide patches if needed. 

For vectors, matrices and bindings to JavaScript TypedArrays I constructed
the packages [purescript-vector](https://github.com/jutaro/purescript-vector), 
[purescript-matrix](https://github.com/jutaro/purescript-matrix) and 
[purescript-typedarray](https://github.com/jutaro/purescript-typedarray),
which are prerequisites of this package.

The first n lessons form the page [learningwebgl](http://learningwebgl.com/blog/) are ported to
purescript in the repo [purescript-webgl-examples](https://github.com/jutaro/purescript-webgl-examples).

I then constructed a slightly higher level interface in the modules Graphics.WebGL
and Graphics.WebGLTexture. I hide the numeric constants behind ADT enums, which guarantee
that you don't pick a value which is not allowed. 

Start up by calling runWebGL with the canvas name to be used, an error handler
and a continuation which takes a context with the canvas name and runs with
the effect EffWebGL. This will fail if the browser does not support webgl. 
 
~~~
runWebGL "glcanvas" (\s -> alert s)
  \ context -> do
~~~

Then you need to define the fshader and vshader in the shader language as two strings.
The Shaders constructor takes the two strings and has an additional phantom type,
which defines the bindings between purescript and the graphic processor. E.g.:
 
~~~
shaders :: Shaders {aVertexPosition :: Attribute Vec3, uPMatrix :: Uniform Mat4, uMVMatrix:: Uniform Mat4}
shaders = Shaders
  """precision mediump float;

  void main(void) {
    gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
      }
  """
  """
      attribute vec3 aVertexPosition;

      uniform mat4 uMVMatrix;
      uniform mat4 uPMatrix;

      void main(void) {
          gl_Position = uPMatrix * uMVMatrix * vec4(aVertexPosition, 1.0);
      }
  """
~~~

Although type safety is guaranteed in the purescript world, currently the correctness of your
types is not checked against the shading language definitions. Be careful as unused variables
in the sahder code will be optimized away and the binding will not be correct then.

By calling 
~~~
withShaders shaders (\s -> alert s)
      \ bindings -> do
~~~
compile the shaders, and the returned bindings are a record, with a type that matches the
shaders phantom type, plus an additional field, which holds the WebGlShaderProgram, which is
needed for some procedures. So here is the withShaders type:

~~~
withShaders :: forall bindings eff a. Shaders (Object bindings) -> (String -> EffWebGL eff a) ->
                ({webGLProgram :: WebGLProg | bindings} -> EffWebGL eff a) -> EffWebGL eff a
~~~

You can then bind values to uniforms and make buffers and draw. This is just on a slightly 
higher leven then calling the original webgl procedures. Eg:

~~~
    let pMatrix = M.makePerspective 45 (canvasWidth / canvasHeight) 0.1 100.0
    setUniformFloats bindings.uPMatrix (M.toArray pMatrix)

    let mvMatrix = M.translate (V.vec3 (-1.5) 0.0 (-7.0))
                      M.identity
    setUniformFloats bindings.uMVMatrix (M.toArray mvMatrix)

    buf1 <- makeBufferFloat [0.0,  1.0,  0.0,
                       (-1.0), (-1.0),  0.0,
                        1.0, (-1.0),  0.0]
    drawArr TRIANGLES buf1 bindings.aVertexPosition
~~~

Textures are supported via the module Graphics.WebGLTexture. A texture can be created by:

~~~
    texture2DFor "crate.gif" MIPMAP \texture -> do
~~~

The interface is not stable, but the basic constructs (effect, shaders) will not be dropped.

Have fun. Jürgen










