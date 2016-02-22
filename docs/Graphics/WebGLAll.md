## Module Graphics.WebGLAll

WebGL binding for purescript


### Re-exported from Control.Monad.Eff.WebGL:

#### `EffWebGL`

``` purescript
type EffWebGL eff a = Eff (webgl :: WebGl | eff) a
```

#### `WebGl`

``` purescript
data WebGl :: !
```

#### `runWebGl_`

``` purescript
runWebGl_ :: forall a e. Eff (webgl :: WebGl | e) a -> Eff e a
```

### Re-exported from Graphics.WebGL:

#### `Attribute`

``` purescript
newtype Attribute typ
  = Attribute { aLocation :: GLint, aName :: String, aItemType :: Int, aItemSize :: Int }
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

#### `Bool`

``` purescript
data Bool
```

#### `Buffer`

``` purescript
type Buffer a = { webGLBuffer :: WebGLBuffer, bufferType :: Int, bufferSize :: Int }
```

#### `BufferTarget`

``` purescript
data BufferTarget
  = ARRAY_BUFFER
  | ELEMENT_ARRAY_BUFFER
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

#### `ContextAttributes`

``` purescript
type ContextAttributes = { alpha :: Boolean, depth :: Boolean, stencil :: Boolean, antialias :: Boolean, premultipliedAlpha :: Boolean, preserveDrawingBuffer :: Boolean, preferLowPowerToHighPerformance :: Boolean, failIfMajorPerformanceCaveat :: Boolean }
```

#### `Float`

``` purescript
data Float
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

#### `Mask`

``` purescript
data Mask
  = DEPTH_BUFFER_BIT
  | STENCIL_BUFFER_BIT
  | COLOR_BUFFER_BIT
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

#### `Sampler2D`

``` purescript
data Sampler2D
```

#### `Shaders`

``` purescript
data Shaders bindings
  = Shaders String String
```

#### `Uniform`

``` purescript
newtype Uniform typ
  = Uniform { uLocation :: WebGLUniformLocation, uName :: String, uType :: Int }
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

#### `WebGLContext`

``` purescript
type WebGLContext = { canvasName :: String }
```

#### `WebGLProg`

``` purescript
newtype WebGLProg
```

#### `bindBuf`

``` purescript
bindBuf :: forall a eff. Buffer a -> Eff (webgl :: WebGl | eff) Unit
```

#### `bindBufAndSetVertexAttr`

``` purescript
bindBufAndSetVertexAttr :: forall a eff typ. Buffer a -> Attribute typ -> Eff (webgl :: WebGl | eff) Unit
```

#### `blendColor`

``` purescript
blendColor :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit
```

#### `blendEquation`

``` purescript
blendEquation :: forall eff. BlendEquation -> Eff (webgl :: WebGl | eff) Unit
```

#### `blendEquationSeparate`

``` purescript
blendEquationSeparate :: forall eff. BlendEquation -> BlendEquation -> Eff (webgl :: WebGl | eff) Unit
```

#### `blendFunc`

``` purescript
blendFunc :: forall eff. BlendingFactor -> BlendingFactor -> Eff (webgl :: WebGl | eff) Unit
```

#### `blendFuncSeparate`

``` purescript
blendFuncSeparate :: forall eff. BlendingFactor -> BlendingFactor -> BlendingFactor -> BlendingFactor -> Eff (webgl :: WebGl | eff) Unit
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

#### `defContextAttributes`

``` purescript
defContextAttributes :: ContextAttributes
```

#### `depthFunc`

``` purescript
depthFunc :: forall eff. Func -> Eff (webgl :: WebGl | eff) Unit
```

#### `disable`

``` purescript
disable :: forall eff. Capacity -> Eff (webgl :: WebGl | eff) Unit
```

#### `disableVertexAttribArray`

``` purescript
disableVertexAttribArray :: forall eff a. Attribute a -> Eff (webgl :: WebGl | eff) Unit
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

#### `enableVertexAttribArray`

``` purescript
enableVertexAttribArray :: forall eff a. Attribute a -> Eff (webgl :: WebGl | eff) Unit
```

#### `fillBuffer`

``` purescript
fillBuffer :: forall a eff. Buffer a -> Int -> Array Number -> Eff (webgl :: WebGl | eff) Unit
```

#### `getCanvasHeight`

``` purescript
getCanvasHeight :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Int
```

#### `getCanvasWidth`

``` purescript
getCanvasWidth :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Int
```

#### `isContextLost`

``` purescript
isContextLost :: forall eff. Eff (webgl :: WebGl | eff) Boolean
```

#### `isEnabled`

``` purescript
isEnabled :: forall eff. Capacity -> Eff (webgl :: WebGl | eff) Boolean
```

#### `makeBuffer`

``` purescript
makeBuffer :: forall a eff num. (ModuloSemiring num) => BufferTarget -> (Array num -> ArrayView a) -> Array num -> Eff (webgl :: WebGl | eff) (Buffer a)
```

#### `makeBufferDyn`

``` purescript
makeBufferDyn :: forall a eff num. (ModuloSemiring num) => BufferTarget -> (Array num -> ArrayView a) -> Array num -> Eff (webgl :: WebGl | eff) (Buffer a)
```

#### `makeBufferFloat`

``` purescript
makeBufferFloat :: forall eff. Array Number -> Eff (webgl :: WebGl | eff) (Buffer Float32)
```

#### `makeBufferFloatDyn`

``` purescript
makeBufferFloatDyn :: forall eff. Array Number -> Eff (webgl :: WebGl | eff) (Buffer Float32)
```

#### `requestAnimationFrame`

``` purescript
requestAnimationFrame :: forall a eff. Eff (webgl :: WebGl | eff) a -> Eff (webgl :: WebGl | eff) Unit
```

#### `runWebGL`

``` purescript
runWebGL :: forall a eff. String -> (String -> Eff eff a) -> (WebGLContext -> EffWebGL eff a) -> Eff eff a
```

Same as runWebGLAttr but uses default attributes (defContextAttributes)

#### `runWebGLAttr`

``` purescript
runWebGLAttr :: forall a eff. String -> ContextAttributes -> (String -> Eff eff a) -> (WebGLContext -> EffWebGL eff a) -> Eff eff a
```

Returns either a continuation which takes a String in the error case,

#### `setUniformBoolean`

``` purescript
setUniformBoolean :: forall eff typ. Uniform typ -> Boolean -> EffWebGL eff Unit
```

#### `setUniformFloats`

``` purescript
setUniformFloats :: forall eff typ. Uniform typ -> Array Number -> EffWebGL eff Unit
```

#### `vertexPointer`

``` purescript
vertexPointer :: forall eff typ. Attribute typ -> EffWebGL eff Unit
```

#### `viewport`

``` purescript
viewport :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit
```

#### `withShaders`

``` purescript
withShaders :: forall bindings eff a. Shaders {  | bindings } -> (String -> EffWebGL eff a) -> ({ webGLProgram :: WebGLProg | bindings } -> EffWebGL eff a) -> EffWebGL eff a
```

### Re-exported from Graphics.WebGLFramebuffer:

#### `AttachementPoint`

``` purescript
data AttachementPoint
  = COLOR_ATTACHMENT0
  | DEPTH_ATTACHMENT
  | STENCIL_ATTACHMENT
  | DEPTH_STENCIL_ATTACHMENT
```

#### `RenderbufferFormat`

``` purescript
data RenderbufferFormat
  = RGBA4
  | RGB565
  | RGB5_A1
  | DEPTH_COMPONENT16
```

#### `WebGLBuf`

``` purescript
newtype WebGLBuf
  = WebGLBuf WebGLFramebuffer
```

#### `WebGLRendBuf`

``` purescript
newtype WebGLRendBuf
  = WebGLRendBuf WebGLRenderbuffer
```

#### `bindFramebuffer`

``` purescript
bindFramebuffer :: forall eff. WebGLBuf -> EffWebGL eff Unit
```

#### `bindRenderbuffer`

``` purescript
bindRenderbuffer :: forall eff. WebGLRendBuf -> EffWebGL eff Unit
```

#### `checkFramebufferStatus`

``` purescript
checkFramebufferStatus :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) GLenum
```

#### `createFramebuffer`

``` purescript
createFramebuffer :: forall eff. EffWebGL eff WebGLBuf
```

#### `createRenderbuffer`

``` purescript
createRenderbuffer :: forall eff. EffWebGL eff WebGLRendBuf
```

#### `framebufferRenderbuffer`

``` purescript
framebufferRenderbuffer :: forall eff. AttachementPoint -> WebGLRendBuf -> EffWebGL eff Unit
```

#### `framebufferTexture2D`

``` purescript
framebufferTexture2D :: forall eff. AttachementPoint -> TargetType -> WebGLTex -> EffWebGL eff Unit
```

#### `readPixels`

``` purescript
readPixels :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> Uint8Array -> Eff (webgl :: WebGl | eff) Uint8Array
```

#### `renderbufferStorage`

``` purescript
renderbufferStorage :: forall eff. RenderbufferFormat -> Int -> Int -> EffWebGL eff Unit
```

#### `unbindFramebuffer`

``` purescript
unbindFramebuffer :: forall eff. EffWebGL eff Unit
```

#### `unbindRenderbuffer`

``` purescript
unbindRenderbuffer :: forall eff. EffWebGL eff Unit
```

### Re-exported from Graphics.WebGLTexture:

#### `InternalFormat`

``` purescript
data InternalFormat
  = IF_ALPHA
  | IF_LUMINANCE
  | IF_LUMINANCE_ALPHA
  | IF_RGB
  | IF_RGBA
```

#### `SymbolicParameter`

``` purescript
data SymbolicParameter
  = PACK_ALIGNMENT
  | UNPACK_ALIGNMENT
  | UNPACK_FLIP_Y_WEBGL
  | UNPACK_PREMULTIPLY_ALPHA_WEBGL
  | UNPACK_COLORSPACE_CONVERSION_WEBGL
```

#### `TargetType`

``` purescript
data TargetType
  = TEXTURE_2D
  | TEXTURE_CUBE_MAP_POSITIVE_X
  | TEXTURE_CUBE_MAP_NEGATIVE_X
  | TEXTURE_CUBE_MAP_POSITIVE_Y
  | TEXTURE_CUBE_MAP_NEGATIVE_Y
  | TEXTURE_CUBE_MAP_POSITIVE_Z
  | TEXTURE_CUBE_MAP_NEGATIVE_Z
```

#### `TexFilterSpec`

``` purescript
data TexFilterSpec
  = NEAREST
  | LINEAR
  | MIPMAP
```

#### `TexParName`

``` purescript
data TexParName
  = TEXTURE_MIN_FILTER
  | TEXTURE_MAG_FILTER
  | TEXTURE_WRAP_S
  | TEXTURE_WRAP_T
```

#### `TexTarget`

``` purescript
data TexTarget
  = TTEXTURE_2D
  | TTEXTURE_CUBE_MAP
```

#### `TextureType`

``` purescript
data TextureType
  = UNSIGNED_BYTE
  | RGBA
  | FLOAT
  | UNSIGNED_SHORT_5_6_5
  | UNSIGNED_SHORT_4_4_4_4
  | UNSIGNED_SHORT_5_5_5_1
```

#### `WebGLTex`

``` purescript
newtype WebGLTex
  = WebGLTex WebGLTexture
```

#### `activeTexture`

``` purescript
activeTexture :: forall eff. Int -> Eff (webgl :: WebGl | eff) Unit
```

#### `bindTexture`

``` purescript
bindTexture :: forall eff. TargetType -> WebGLTex -> EffWebGL eff Unit
```

#### `createTexture`

``` purescript
createTexture :: forall eff. Eff (webgl :: WebGl | eff) WebGLTex
```

#### `handleLoad2D`

``` purescript
handleLoad2D :: forall eff a. WebGLTex -> TexFilterSpec -> a -> EffWebGL eff Unit
```

#### `newTexture`

``` purescript
newTexture :: forall eff. Int -> Int -> TexFilterSpec -> EffWebGL eff WebGLTex
```

#### `targetTypeToConst`

``` purescript
targetTypeToConst :: TargetType -> GLenum
```

#### `texture2DFor`

``` purescript
texture2DFor :: forall a eff. String -> TexFilterSpec -> (WebGLTex -> EffWebGL eff a) -> EffWebGL eff Unit
```

#### `unbindTexture`

``` purescript
unbindTexture :: forall eff. TargetType -> EffWebGL eff Unit
```

#### `withTexture2D`

``` purescript
withTexture2D :: forall eff typ. WebGLTex -> Int -> Uniform typ -> Int -> EffWebGL eff Unit
```

