## Module Graphics.WebGL

WebGL binding for purescript

#### `WebGLContext`

```purescript
type WebGLContext = { canvasName :: String }
```

#### `ContextAttributes`

```purescript
type ContextAttributes = { alpha :: Boolean, depth :: Boolean, stencil :: Boolean, antialias :: Boolean, premultipliedAlpha :: Boolean, preserveDrawingBuffer :: Boolean, preferLowPowerToHighPerformance :: Boolean, failIfMajorPerformanceCaveat :: Boolean }
```

#### `defContextAttributes`

```purescript
defContextAttributes :: ContextAttributes
```

#### `runWebGLAttr`

```purescript
runWebGLAttr :: forall a.  String -> ContextAttributes -> (String -> Effect a) -> (WebGLContext -> Effect a) -> Effect a
```

pures either a continuation which takes a String in the error case,

#### `runWebGL`

```purescript
runWebGL :: forall a.  String -> (String -> Effect a) -> (WebGLContext -> Effect a) -> Effect a
```

Same as runWebGLAttr but uses default attributes (defContextAttributes)

#### `Uniform`

```purescript
newtype Uniform typ
  = Uniform { uLocation :: WebGLUniformLocation, uName :: String, uType :: Int }
```

#### `Attribute`

```purescript
newtype Attribute typ
  = Attribute { aLocation :: GLint, aName :: String, aItemType :: Int, aItemSize :: Int }
```

#### `Vec2`

```purescript
data Vec2
```

#### `Vec3`

```purescript
data Vec3
```

#### `Vec4`

```purescript
data Vec4
```

#### `Mat2`

```purescript
data Mat2
```

#### `Mat3`

```purescript
data Mat3
```

#### `Mat4`

```purescript
data Mat4
```

#### `Sampler2D`

```purescript
data Sampler2D
```

#### `Bool`

```purescript
data Bool
```

#### `Float`

```purescript
data Float
```

#### `WebGLProg`

```purescript
newtype WebGLProg
```

#### `Shaders`

```purescript
data Shaders bindings
  = Shaders String String
```

#### `requestAnimationFrame`

```purescript
requestAnimationFrame :: forall a.  Effect a -> Effect Unit
```

#### `withShaders`

```purescript
withShaders :: forall bindings a. Shaders ({  | bindings }) -> (String -> Effect a) -> ({ webGLProgram :: WebGLProg | bindings } -> Effect a) -> Effect a
```

#### `bindAttribLocation`

```purescript
bindAttribLocation :: WebGLProg -> Int -> String -> Effect Unit
```

#### `Buffer`

```purescript
type Buffer a = { webGLBuffer :: WebGLBuffer, bufferType :: Int, bufferSize :: Int }
```

#### `makeBufferFloat`

```purescript
makeBufferFloat :: Array Number -> Effect (Buffer Float32)
```

#### `makeBufferFloatDyn`

```purescript
makeBufferFloatDyn :: Array Number -> Effect (Buffer Float32)
```

#### `makeBuffer`

```purescript
makeBuffer :: forall a num. EuclideanRing num => BufferTarget -> (Array num -> ArrayView a) -> Array num -> Effect (Buffer a)
```

#### `makeBufferDyn`

```purescript
makeBufferDyn :: forall a num. EuclideanRing num => BufferTarget -> (Array num -> ArrayView a) -> Array num -> Effect (Buffer a)
```

#### `fillBuffer`

```purescript
fillBuffer :: forall a.  Buffer a -> Int -> Array Number -> Effect Unit
```

#### `setUniformFloats`

```purescript
setUniformFloats :: forall typ. Uniform typ -> Array Number -> Effect Unit
```

#### `setUniformBoolean`

```purescript
setUniformBoolean :: forall typ. Uniform typ -> Boolean -> Effect Unit
```

#### `bindBufAndSetVertexAttr`

```purescript
bindBufAndSetVertexAttr :: forall a typ. Buffer a -> Attribute typ -> Effect Unit
```

#### `bindBuf`

```purescript
bindBuf :: forall a.  Buffer a -> Effect Unit
```

#### `blendColor`

```purescript
blendColor :: GLclampf -> GLclampf -> GLclampf -> GLclampf -> Effect Unit
```

#### `blendFunc`

```purescript
blendFunc :: BlendingFactor -> BlendingFactor -> (Effect Unit)
```

#### `blendFuncSeparate`

```purescript
blendFuncSeparate :: BlendingFactor -> BlendingFactor -> BlendingFactor -> BlendingFactor -> (Effect Unit)
```

#### `blendEquation`

```purescript
blendEquation :: BlendEquation -> (Effect Unit)
```

#### `blendEquationSeparate`

```purescript
blendEquationSeparate :: BlendEquation -> BlendEquation -> (Effect Unit)
```

#### `clear`

```purescript
clear :: Array Mask -> (Effect Unit)
```

#### `clearColor`

```purescript
clearColor :: GLclampf -> GLclampf -> GLclampf -> GLclampf -> Effect Unit
```

#### `clearDepth`

```purescript
clearDepth :: GLclampf -> Effect Unit
```

#### `clearStencil`

```purescript
clearStencil :: GLint -> Effect Unit
```

#### `colorMask`

```purescript
colorMask :: GLboolean -> GLboolean -> GLboolean -> GLboolean -> Effect Unit
```

#### `Func`

```purescript
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

```purescript
depthFunc :: Func -> Effect Unit
```

#### `disable`

```purescript
disable :: Capacity -> (Effect Unit)
```

#### `drawArr`

```purescript
drawArr :: forall a typ. Mode -> Buffer a -> Attribute typ -> Effect Unit
```

#### `drawElements`

```purescript
drawElements :: Mode -> Int -> Effect Unit
```

#### `enable`

```purescript
enable :: Capacity -> (Effect Unit)
```

#### `isContextLost`

```purescript
isContextLost :: Effect Boolean
```

#### `isEnabled`

```purescript
isEnabled :: Capacity -> (Effect Boolean)
```

#### `vertexPointer`

```purescript
vertexPointer :: forall typ. Attribute typ -> Effect Unit
```

#### `viewport`

```purescript
viewport :: GLint -> GLint -> GLsizei -> GLsizei -> Effect Unit
```

#### `enableVertexAttribArray`

```purescript
enableVertexAttribArray :: forall a. Attribute a -> (Effect Unit)
```

#### `disableVertexAttribArray`

```purescript
disableVertexAttribArray :: forall a. Attribute a -> (Effect Unit)
```

#### `getCanvasWidth`

```purescript
getCanvasWidth :: WebGLContext -> Effect Int
```

#### `getCanvasHeight`

```purescript
getCanvasHeight :: WebGLContext -> Effect Int
```

#### `Capacity`

```purescript
data Capacity
  = BLEND
  | DEPTH_TEST
  | CULL_FACE
  | POLYGON_OFFSET_FILL
  | SCISSOR_TEST
```

#### `Mask`

```purescript
data Mask
  = DEPTH_BUFFER_BIT
  | STENCIL_BUFFER_BIT
  | COLOR_BUFFER_BIT
```

#### `Mode`

```purescript
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

```purescript
data BufferTarget
  = ARRAY_BUFFER
  | ELEMENT_ARRAY_BUFFER
```

#### `BlendingFactor`

```purescript
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

```purescript
data BlendEquation
  = FUNC_ADD
  | BLEND_EQUATION
  | BLEND_EQUATION_RGB
  | BLEND_EQUATION_ALPHA
  | FUNC_SUBTRACT
  | FUNC_REVERSE_SUBTRACT
```
