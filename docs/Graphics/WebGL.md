## Module Graphics.WebGL

#### `WebGLContext`

``` purescript
type WebGLContext = { canvasName :: String }
```

#### `ContextAttributes`

``` purescript
type ContextAttributes = { alpha :: Boolean, depth :: Boolean, stencil :: Boolean, antialias :: Boolean, premultipliedAlpha :: Boolean, preserveDrawingBuffer :: Boolean, preferLowPowerToHighPerformance :: Boolean, failIfMajorPerformanceCaveat :: Boolean }
```

#### `defContextAttributes`

``` purescript
defContextAttributes :: ContextAttributes
```

#### `runWebGLAttr`

``` purescript
runWebGLAttr :: forall a eff. String -> ContextAttributes -> (String -> Eff eff a) -> (WebGLContext -> EffWebGL eff a) -> Eff eff a
```

#### `runWebGL`

``` purescript
runWebGL :: forall a eff. String -> (String -> Eff eff a) -> (WebGLContext -> EffWebGL eff a) -> Eff eff a
```

Same as runWebGLAttr but uses default attributes (defContextAttributes)

#### `Uniform`

``` purescript
newtype Uniform typ
  = Uniform { uLocation :: WebGLUniformLocation, uName :: String, uType :: Int }
```

#### `Attribute`

``` purescript
newtype Attribute typ
  = Attribute { aLocation :: GLint, aName :: String, aItemType :: Int, aItemSize :: Int }
```

#### `Vec2`

``` purescript
data Vec2
```

#### `Vec3`

``` purescript
data Vec3
```

#### `Vec4`

``` purescript
data Vec4
```

#### `Mat2`

``` purescript
data Mat2
```

#### `Mat3`

``` purescript
data Mat3
```

#### `Mat4`

``` purescript
data Mat4
```

#### `Sampler2D`

``` purescript
data Sampler2D
```

#### `Bool`

``` purescript
data Bool
```

#### `Float`

``` purescript
data Float
```

#### `WebGLProg`

``` purescript
newtype WebGLProg
```

#### `Shaders`

``` purescript
data Shaders bindings
  = Shaders String String
```

#### `requestAnimationFrame`

``` purescript
requestAnimationFrame :: forall a eff. Eff (webgl :: WebGl | eff) a -> Eff (webgl :: WebGl | eff) Unit
```

#### `withShaders`

``` purescript
withShaders :: forall bindings eff a. Shaders {  | bindings } -> (String -> EffWebGL eff a) -> ({ webGLProgram :: WebGLProg | bindings } -> EffWebGL eff a) -> EffWebGL eff a
```

#### `Buffer`

``` purescript
type Buffer a = { webGLBuffer :: WebGLBuffer, bufferType :: Int, bufferSize :: Int }
```

#### `makeBufferFloat`

``` purescript
makeBufferFloat :: forall eff. Array Number -> Eff (webgl :: WebGl | eff) (Buffer Float32)
```

#### `makeBufferFloatDyn`

``` purescript
makeBufferFloatDyn :: forall eff. Array Number -> Eff (webgl :: WebGl | eff) (Buffer Float32)
```

#### `makeBuffer`

``` purescript
makeBuffer :: forall a eff num. (ModuloSemiring num) => BufferTarget -> (Array num -> ArrayView a) -> Array num -> Eff (webgl :: WebGl | eff) (Buffer a)
```

#### `makeBufferDyn`

``` purescript
makeBufferDyn :: forall a eff num. (ModuloSemiring num) => BufferTarget -> (Array num -> ArrayView a) -> Array num -> Eff (webgl :: WebGl | eff) (Buffer a)
```

#### `fillBuffer`

``` purescript
fillBuffer :: forall a eff. Buffer a -> Int -> Array Number -> Eff (webgl :: WebGl | eff) Unit
```

#### `setUniformFloats`

``` purescript
setUniformFloats :: forall eff typ. Uniform typ -> Array Number -> EffWebGL eff Unit
```

#### `setUniformBoolean`

``` purescript
setUniformBoolean :: forall eff typ. Uniform typ -> Boolean -> EffWebGL eff Unit
```

#### `bindBufAndSetVertexAttr`

``` purescript
bindBufAndSetVertexAttr :: forall a eff typ. Buffer a -> Attribute typ -> Eff (webgl :: WebGl | eff) Unit
```

#### `bindBuf`

``` purescript
bindBuf :: forall a eff. Buffer a -> Eff (webgl :: WebGl | eff) Unit
```

#### `blendColor`

``` purescript
blendColor :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit
```

#### `blendFunc`

``` purescript
blendFunc :: forall eff. BlendingFactor -> BlendingFactor -> Eff (webgl :: WebGl | eff) Unit
```

#### `blendFuncSeparate`

``` purescript
blendFuncSeparate :: forall eff. BlendingFactor -> BlendingFactor -> BlendingFactor -> BlendingFactor -> Eff (webgl :: WebGl | eff) Unit
```

#### `blendEquation`

``` purescript
blendEquation :: forall eff. BlendEquation -> Eff (webgl :: WebGl | eff) Unit
```

#### `blendEquationSeparate`

``` purescript
blendEquationSeparate :: forall eff. BlendEquation -> BlendEquation -> Eff (webgl :: WebGl | eff) Unit
```

#### `clear`

``` purescript
clear :: forall eff. Array Mask -> Eff (webgl :: WebGl | eff) Unit
```

#### `clearColor`

``` purescript
clearColor :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit
```

#### `clearDepth`

``` purescript
clearDepth :: forall eff. GLclampf -> Eff (webgl :: WebGl | eff) Unit
```

#### `clearStencil`

``` purescript
clearStencil :: forall eff. GLint -> Eff (webgl :: WebGl | eff) Unit
```

#### `colorMask`

``` purescript
colorMask :: forall eff. GLboolean -> GLboolean -> GLboolean -> GLboolean -> Eff (webgl :: WebGl | eff) Unit
```

#### `Func`

``` purescript
data Func
  = NEVER
  | ALWAYS
  | LESS
  | EQUAL
  | LEQUAL
  | GREATER
  | GEQUAL
  | NOTEQUAL
```

#### `depthFunc`

``` purescript
depthFunc :: forall eff. Func -> Eff (webgl :: WebGl | eff) Unit
```

#### `disable`

``` purescript
disable :: forall eff. Capacity -> Eff (webgl :: WebGl | eff) Unit
```

#### `drawArr`

``` purescript
drawArr :: forall a eff typ. Mode -> Buffer a -> Attribute typ -> EffWebGL eff Unit
```

#### `drawElements`

``` purescript
drawElements :: forall eff. Mode -> Int -> EffWebGL eff Unit
```

#### `enable`

``` purescript
enable :: forall eff. Capacity -> Eff (webgl :: WebGl | eff) Unit
```

#### `isContextLost`

``` purescript
isContextLost :: forall eff. Eff (webgl :: WebGl | eff) Boolean
```

#### `isEnabled`

``` purescript
isEnabled :: forall eff. Capacity -> Eff (webgl :: WebGl | eff) Boolean
```

#### `vertexPointer`

``` purescript
vertexPointer :: forall eff typ. Attribute typ -> EffWebGL eff Unit
```

#### `viewport`

``` purescript
viewport :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit
```

#### `enableVertexAttribArray`

``` purescript
enableVertexAttribArray :: forall eff a. Attribute a -> Eff (webgl :: WebGl | eff) Unit
```

#### `disableVertexAttribArray`

``` purescript
disableVertexAttribArray :: forall eff a. Attribute a -> Eff (webgl :: WebGl | eff) Unit
```

#### `getCanvasWidth`

``` purescript
getCanvasWidth :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Int
```

#### `getCanvasHeight`

``` purescript
getCanvasHeight :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Int
```

#### `Capacity`

``` purescript
data Capacity
  = BLEND
  | DEPTH_TEST
  | CULL_FACE
  | POLYGON_OFFSET_FILL
  | SCISSOR_TEST
```

#### `Mask`

``` purescript
data Mask
  = DEPTH_BUFFER_BIT
  | STENCIL_BUFFER_BIT
  | COLOR_BUFFER_BIT
```

#### `Mode`

``` purescript
data Mode
  = POINTS
  | LINES
  | LINE_STRIP
  | LINE_LOOP
  | TRIANGLES
  | TRIANGLE_STRIP
  | TRIANGLE_FAN
```

#### `BufferTarget`

``` purescript
data BufferTarget
  = ARRAY_BUFFER
  | ELEMENT_ARRAY_BUFFER
```

#### `BlendingFactor`

``` purescript
data BlendingFactor
  = ZERO
  | ONE
  | SRC_COLOR
  | ONE_MINUS_SRC_COLOR
  | DST_COLOR
  | ONE_MINUS_DST_COLOR
  | SRC_ALPHA
  | ONE_MINUS_SRC_ALPHA
  | DST_ALPHA
  | ONE_MINUS_DST_ALPHA
  | SRC_ALPHA_SATURATE
  | BLEND_DST_RGB
  | BLEND_SRC_RGB
  | BLEND_DST_ALPHA
  | BLEND_SRC_ALPHA
  | CONSTANT_COLOR
  | ONE_MINUS_CONSTANT_COLOR
  | CONSTANT_ALPHA
  | ONE_MINUS_CONSTANT_ALPHA
  | BLEND_COLOR
```

#### `BlendEquation`

``` purescript
data BlendEquation
  = FUNC_ADD
  | BLEND_EQUATION
  | BLEND_EQUATION_RGB
  | BLEND_EQUATION_ALPHA
  | FUNC_SUBTRACT
  | FUNC_REVERSE_SUBTRACT
```


