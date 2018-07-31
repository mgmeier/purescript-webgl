## Module Graphics.WebGLAll

WebGL binding for purescript

### Re-exported from Effect.WebGL:

#### `WebGl`

```purescript
data WebGl :: !
```

#### `runWebGl_`

```purescript
runWebGl_ :: forall a. Effect a -> Effect a
```

### Re-exported from Graphics.WebGL:

#### `WebGLProg`

```purescript
newtype WebGLProg
```

#### `WebGLContext`

```purescript
type WebGLContext = { canvasName :: String }
```

#### `Vec4`

```purescript
data Vec4
```

#### `Vec3`

```purescript
data Vec3
```

#### `Vec2`

```purescript
data Vec2
```

#### `Uniform`

```purescript
newtype Uniform typ
  = Uniform { uLocation :: WebGLUniformLocation, uName :: String, uType :: Int }
```

#### `Shaders`

```purescript
data Shaders bindings
  = Shaders String String
```

#### `Sampler2D`

```purescript
data Sampler2D
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

#### `Mat4`

```purescript
data Mat4
```

#### `Mat3`

```purescript
data Mat3
```

#### `Mat2`

```purescript
data Mat2
```

#### `Mask`

```purescript
data Mask
  = DEPTH_BUFFER_BIT
  | STENCIL_BUFFER_BIT
  | COLOR_BUFFER_BIT
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

#### `Float`

```purescript
data Float
```

#### `ContextAttributes`

```purescript
type ContextAttributes = { alpha :: Boolean, depth :: Boolean, stencil :: Boolean, antialias :: Boolean, premultipliedAlpha :: Boolean, preserveDrawingBuffer :: Boolean, preferLowPowerToHighPerformance :: Boolean, failIfMajorPerformanceCaveat :: Boolean }
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

#### `BufferTarget`

```purescript
data BufferTarget
  = ARRAY_BUFFER
  | ELEMENT_ARRAY_BUFFER
```

#### `Buffer`

```purescript
type Buffer a = { webGLBuffer :: WebGLBuffer, bufferType :: Int, bufferSize :: Int }
```

#### `Bool`

```purescript
data Bool
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

#### `Attribute`

```purescript
newtype Attribute typ
  = Attribute { aLocation :: GLint, aName :: String, aItemType :: Int, aItemSize :: Int }
```

#### `withShaders`

```purescript
withShaders :: forall bindings a. Shaders ({  | bindings }) -> (String -> Effect a) -> ({ webGLProgram :: WebGLProg | bindings } -> Effect a) -> Effect a
```

#### `viewport`

```purescript
viewport :: GLint -> GLint -> GLsizei -> GLsizei -> Effect Unit
```

#### `vertexPointer`

```purescript
vertexPointer :: forall typ. Attribute typ -> Effect Unit
```

#### `setUniformFloats`

```purescript
setUniformFloats :: forall typ. Uniform typ -> Array Number -> Effect Unit
```

#### `setUniformBoolean`

```purescript
setUniformBoolean :: forall typ. Uniform typ -> Boolean -> Effect Unit
```

#### `runWebGLAttr`

```purescript
runWebGLAttr :: forall a. String -> ContextAttributes -> (String -> Effect a) -> (WebGLContext -> Effect a) -> Effect a
```

pures either a continuation which takes a String in the error case,

#### `runWebGL`

```purescript
runWebGL :: forall a. String -> (String -> Effect a) -> (WebGLContext -> Effect a) -> Effect a
```

Same as runWebGLAttr but uses default attributes (defContextAttributes)

#### `requestAnimationFrame`

```purescript
requestAnimationFrame :: forall a. Effect a -> Effect Unit
```

#### `makeBufferFloatDyn`

```purescript
makeBufferFloatDyn :: Array Number -> Effect (Buffer Float32)
```

#### `makeBufferFloat`

```purescript
makeBufferFloat :: Array Number -> Effect (Buffer Float32)
```

#### `makeBufferDyn`

```purescript
makeBufferDyn :: forall a num. EuclideanRing num => BufferTarget -> (Array num -> ArrayView a) -> Array num -> Effect (Buffer a)
```

#### `makeBuffer`

```purescript
makeBuffer :: forall a num. EuclideanRing num => BufferTarget -> (Array num -> ArrayView a) -> Array num -> Effect (Buffer a)
```

#### `isEnabled`

```purescript
isEnabled :: forall . Capacity -> (Effect Boolean)
```

#### `isContextLost`

```purescript
isContextLost :: Effect Boolean
```

#### `getCanvasWidth`

```purescript
getCanvasWidth :: WebGLContext -> Effect Int
```

#### `getCanvasHeight`

```purescript
getCanvasHeight :: WebGLContext -> Effect Int
```

#### `fillBuffer`

```purescript
fillBuffer :: forall a. Buffer a -> Int -> Array Number -> Effect Unit
```

#### `enableVertexAttribArray`

```purescript
enableVertexAttribArray :: forall a. Attribute a -> (Effect Unit)
```

#### `enable`

```purescript
enable :: forall . Capacity -> (Effect Unit)
```

#### `drawElements`

```purescript
drawElements :: forall. Mode -> Int -> Effect Unit
```

#### `drawArr`

```purescript
drawArr :: forall a typ. Mode -> Buffer a -> Attribute typ -> Effect Unit
```

#### `disableVertexAttribArray`

```purescript
disableVertexAttribArray :: forall a. Attribute a -> (Effect Unit)
```

#### `disable`

```purescript
disable :: Capacity -> (Effect Unit)
```

#### `depthFunc`

```purescript
depthFunc :: Func -> Effect Unit
```

#### `defContextAttributes`

```purescript
defContextAttributes :: ContextAttributes
```

#### `colorMask`

```purescript
colorMask :: GLboolean -> GLboolean -> GLboolean -> GLboolean -> Effect Unit
```

#### `clearStencil`

```purescript
clearStencil :: GLint -> Effect Unit
```

#### `clearDepth`

```purescript
clearDepth :: GLclampf -> Effect Unit
```

#### `clearColor`

```purescript
clearColor :: GLclampf -> GLclampf -> GLclampf -> GLclampf -> Effect Unit
```

#### `clear`

```purescript
clear :: Array Mask -> (Effect Unit)
```

#### `blendFuncSeparate`

```purescript
blendFuncSeparate :: BlendingFactor -> BlendingFactor -> BlendingFactor -> BlendingFactor -> (Effect Unit)
```

#### `blendFunc`

```purescript
blendFunc :: BlendingFactor -> BlendingFactor -> (Effect Unit)
```

#### `blendEquationSeparate`

```purescript
blendEquationSeparate :: BlendEquation -> BlendEquation -> (Effect Unit)
```

#### `blendEquation`

```purescript
blendEquation :: BlendEquation -> (Effect Unit)
```

#### `blendColor`

```purescript
blendColor :: GLclampf -> GLclampf -> GLclampf -> GLclampf -> Effect Unit
```

#### `bindBufAndSetVertexAttr`

```purescript
bindBufAndSetVertexAttr :: forall a typ. Buffer a -> Attribute typ -> Effect Unit
```

#### `bindBuf`

```purescript
bindBuf :: forall a. Buffer a -> Effect Unit
```

#### `bindAttribLocation`

```purescript
bindAttribLocation :: WebGLProg -> Int -> String -> Effect Unit
```

### Re-exported from Graphics.WebGLFramebuffer:

#### `WebGLRendBuf`

```purescript
newtype WebGLRendBuf
  = WebGLRendBuf WebGLRenderbuffer
```

#### `WebGLBuf`

```purescript
newtype WebGLBuf
  = WebGLBuf WebGLFramebuffer
```

#### `RenderbufferFormat`

```purescript
data RenderbufferFormat
  = RGBA4
  | RGB565
  | RGB5_A1
  | DEPTH_COMPONENT16
```

#### `FrameBufferCode`

```purescript
data FrameBufferCode
  = FRAMEBUFFER_COMPLETE
  | FRAMEBUFFER_INCOMPLETE_ATTACHMENT
  | FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
  | FRAMEBUFFER_INCOMPLETE_DIMENSIONS
  | FRAMEBUFFER_UNSUPPORTED
```

#### `AttachementPoint`

```purescript
data AttachementPoint
  = COLOR_ATTACHMENT0
  | DEPTH_ATTACHMENT
  | STENCIL_ATTACHMENT
  | DEPTH_STENCIL_ATTACHMENT
```

#### `unbindRenderbuffer`

```purescript
unbindRenderbuffer :: Effect Unit
```

#### `unbindFramebuffer`

```purescript
unbindFramebuffer :: Effect Unit
```

#### `renderbufferStorage`

```purescript
renderbufferStorage :: RenderbufferFormat -> Int -> Int -> Effect Unit
```

#### `readPixels`

```purescript
readPixels :: GLint -> GLint -> GLsizei -> GLsizei -> Uint8Array -> Effect Uint8Array
```

#### `framebufferTexture2D`

```purescript
framebufferTexture2D :: AttachementPoint -> TargetType -> WebGLTex -> Effect Unit
```

#### `framebufferRenderbuffer`

```purescript
framebufferRenderbuffer :: AttachementPoint -> WebGLRendBuf -> Effect Unit
```

#### `frameBufferCodeToConst`

```purescript
frameBufferCodeToConst :: FrameBufferCode -> GLenum
```

#### `createRenderbuffer`

```purescript
createRenderbuffer :: Effect WebGLRendBuf
```

#### `createFramebuffer`

```purescript
createFramebuffer :: Effect WebGLBuf
```

#### `checkFramebufferStatus`

```purescript
checkFramebufferStatus :: GLenum -> Effect GLenum
```

#### `bindRenderbuffer`

```purescript
bindRenderbuffer :: WebGLRendBuf -> Effect Unit
```

#### `bindFramebuffer`

```purescript
bindFramebuffer :: WebGLBuf -> Effect Unit
```

### Re-exported from Graphics.WebGLTexture:

#### `WebGLTex`

```purescript
newtype WebGLTex
  = WebGLTex WebGLTexture
```

#### `TextureType`

```purescript
data TextureType
  = UNSIGNED_BYTE
  | RGBA
  | FLOAT
  | UNSIGNED_SHORT_5_6_5
  | UNSIGNED_SHORT_4_4_4_4
  | UNSIGNED_SHORT_5_5_5_1
```

#### `TexTarget`

```purescript
data TexTarget
  = TTEXTURE_2D
  | TTEXTURE_CUBE_MAP
```

#### `TexParName`

```purescript
data TexParName
  = TEXTURE_MIN_FILTER
  | TEXTURE_MAG_FILTER
  | TEXTURE_WRAP_S
  | TEXTURE_WRAP_T
```

#### `TexFilterSpec`

```purescript
data TexFilterSpec
  = NEAREST
  | LINEAR
  | MIPMAP
```

#### `TargetType`

```purescript
data TargetType
  = TEXTURE_2D
  | TEXTURE_CUBE_MAP_POSITIVE_X
  | TEXTURE_CUBE_MAP_NEGATIVE_X
  | TEXTURE_CUBE_MAP_POSITIVE_Y
  | TEXTURE_CUBE_MAP_NEGATIVE_Y
  | TEXTURE_CUBE_MAP_POSITIVE_Z
  | TEXTURE_CUBE_MAP_NEGATIVE_Z
```

#### `SymbolicParameter`

```purescript
data SymbolicParameter
  = PACK_ALIGNMENT
  | UNPACK_ALIGNMENT
  | UNPACK_FLIP_Y_WEBGL
  | UNPACK_PREMULTIPLY_ALPHA_WEBGL
  | UNPACK_COLORSPACE_CONVERSION_WEBGL
```

#### `InternalFormat`

```purescript
data InternalFormat
  = IF_ALPHA
  | IF_LUMINANCE
  | IF_LUMINANCE_ALPHA
  | IF_RGB
  | IF_RGBA
```

#### `withTexture2D`

```purescript
withTexture2D :: forall typ. WebGLTex -> Int -> Uniform typ -> Int -> Effect Unit -> Effect Unit
```

#### `unbindTexture`

```purescript
unbindTexture :: TargetType -> Effect Unit
```

#### `texture2DFor`

```purescript
texture2DFor :: forall a. String -> TexFilterSpec -> (WebGLTex -> Effect a) -> Effect Unit
```

#### `targetTypeToConst`

```purescript
targetTypeToConst :: TargetType -> GLenum
```

#### `newTextureInit`

```purescript
newTextureInit :: Int -> Int -> TexFilterSpec -> Effect WebGLTex
```

#### `newTexture`

```purescript
newTexture :: Int -> Int -> TexFilterSpec -> Effect WebGLTex
```

#### `handleSubLoad2D`

```purescript
handleSubLoad2D :: forall a. WebGLTex -> Int -> Int -> Int -> Int -> TexFilterSpec -> a -> Effect Unit
```

#### `handleLoad2D`

```purescript
handleLoad2D :: forall a. WebGLTex -> TexFilterSpec -> a -> Effect Unit
```

#### `createTexture`

```purescript
createTexture :: Effect WebGLTex
```

#### `bindTexture`

```purescript
bindTexture :: TargetType -> WebGLTex -> Effect Unit
```

#### `activeTexture`

```purescript
activeTexture :: Int -> Effect Unit
```
