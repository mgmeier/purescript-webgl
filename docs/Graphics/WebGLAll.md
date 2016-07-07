## Module Graphics.WebGLAll

WebGL binding for purescript


### Re-exported from Control.Monad.Eff.WebGL:

#### `WebGl`

``` purescript
data WebGl :: !
```

#### `EffWebGL`

``` purescript
type EffWebGL eff a = Eff (webgl :: WebGl | eff) a
```

#### `runWebGl_`

``` purescript
runWebGl_ :: forall a e. Eff (webgl :: WebGl | e) a -> Eff e a
```

### Re-exported from Graphics.WebGL:

#### `WebGLProg`

``` purescript
newtype WebGLProg
```

#### `WebGLContext`

``` purescript
type WebGLContext = { canvasName :: String }
```

#### `Vec4`

``` purescript
data Vec4
```

#### `Vec3`

``` purescript
data Vec3
```

#### `Vec2`

``` purescript
data Vec2
```

#### `Uniform`

``` purescript
newtype Uniform typ
  = Uniform { uLocation :: WebGLUniformLocation, uName :: String, uType :: Int }
```

#### `Shaders`

``` purescript
data Shaders bindings
  = Shaders String String
```

#### `Sampler2D`

``` purescript
data Sampler2D
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

#### `Mat4`

``` purescript
data Mat4
```

#### `Mat3`

``` purescript
data Mat3
```

#### `Mat2`

``` purescript
data Mat2
```

#### `Mask`

``` purescript
data Mask
  = DEPTH_BUFFER_BIT
  | STENCIL_BUFFER_BIT
  | COLOR_BUFFER_BIT
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

#### `Float`

``` purescript
data Float
```

#### `ContextAttributes`

``` purescript
type ContextAttributes = { alpha :: Boolean, depth :: Boolean, stencil :: Boolean, antialias :: Boolean, premultipliedAlpha :: Boolean, preserveDrawingBuffer :: Boolean, preferLowPowerToHighPerformance :: Boolean, failIfMajorPerformanceCaveat :: Boolean }
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

#### `BufferTarget`

``` purescript
data BufferTarget
  = ARRAY_BUFFER
  | ELEMENT_ARRAY_BUFFER
```

#### `Buffer`

``` purescript
type Buffer a = { webGLBuffer :: WebGLBuffer, bufferType :: Int, bufferSize :: Int }
```

#### `Bool`

``` purescript
data Bool
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

#### `Attribute`

``` purescript
newtype Attribute typ
  = Attribute { aLocation :: GLint, aName :: String, aItemType :: Int, aItemSize :: Int }
```

#### `withShaders`

``` purescript
withShaders :: forall bindings eff a. Shaders ({  | bindings }) -> (String -> EffWebGL eff a) -> ({ webGLProgram :: WebGLProg | bindings } -> EffWebGL eff a) -> EffWebGL eff a
```

#### `viewport`

``` purescript
viewport :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit
```

#### `vertexPointer`

``` purescript
vertexPointer :: forall eff typ. Attribute typ -> EffWebGL eff Unit
```

#### `setUniformFloats`

``` purescript
setUniformFloats :: forall eff typ. Uniform typ -> Array Number -> EffWebGL eff Unit
```

#### `setUniformBoolean`

``` purescript
setUniformBoolean :: forall eff typ. Uniform typ -> Boolean -> EffWebGL eff Unit
```

#### `runWebGLAttr`

``` purescript
runWebGLAttr :: forall a eff. String -> ContextAttributes -> (String -> Eff eff a) -> (WebGLContext -> EffWebGL eff a) -> Eff eff a
```

pures either a continuation which takes a String in the error case,

#### `runWebGL`

``` purescript
runWebGL :: forall a eff. String -> (String -> Eff eff a) -> (WebGLContext -> EffWebGL eff a) -> Eff eff a
```

Same as runWebGLAttr but uses default attributes (defContextAttributes)

#### `requestAnimationFrame`

``` purescript
requestAnimationFrame :: forall a eff. Eff (webgl :: WebGl | eff) a -> Eff (webgl :: WebGl | eff) Unit
```

#### `makeBufferFloatDyn`

``` purescript
makeBufferFloatDyn :: forall eff. Array Number -> Eff (webgl :: WebGl | eff) (Buffer Float32)
```

#### `makeBufferFloat`

``` purescript
makeBufferFloat :: forall eff. Array Number -> Eff (webgl :: WebGl | eff) (Buffer Float32)
```

#### `makeBufferDyn`

``` purescript
makeBufferDyn :: forall a eff num. EuclideanRing num => BufferTarget -> (Array num -> ArrayView a) -> Array num -> Eff (webgl :: WebGl | eff) (Buffer a)
```

#### `makeBuffer`

``` purescript
makeBuffer :: forall a eff num. EuclideanRing num => BufferTarget -> (Array num -> ArrayView a) -> Array num -> Eff (webgl :: WebGl | eff) (Buffer a)
```

#### `isEnabled`

``` purescript
isEnabled :: forall eff. Capacity -> (Eff (webgl :: WebGl | eff) Boolean)
```

#### `isContextLost`

``` purescript
isContextLost :: forall eff. Eff (webgl :: WebGl | eff) Boolean
```

#### `getCanvasWidth`

``` purescript
getCanvasWidth :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Int
```

#### `getCanvasHeight`

``` purescript
getCanvasHeight :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Int
```

#### `fillBuffer`

``` purescript
fillBuffer :: forall a eff. Buffer a -> Int -> Array Number -> Eff (webgl :: WebGl | eff) Unit
```

#### `enableVertexAttribArray`

``` purescript
enableVertexAttribArray :: forall eff a. Attribute a -> (Eff (webgl :: WebGl | eff) Unit)
```

#### `enable`

``` purescript
enable :: forall eff. Capacity -> (Eff (webgl :: WebGl | eff) Unit)
```

#### `drawElements`

``` purescript
drawElements :: forall eff. Mode -> Int -> EffWebGL eff Unit
```

#### `drawArr`

``` purescript
drawArr :: forall a eff typ. Mode -> Buffer a -> Attribute typ -> EffWebGL eff Unit
```

#### `disableVertexAttribArray`

``` purescript
disableVertexAttribArray :: forall eff a. Attribute a -> (Eff (webgl :: WebGl | eff) Unit)
```

#### `disable`

``` purescript
disable :: forall eff. Capacity -> (Eff (webgl :: WebGl | eff) Unit)
```

#### `depthFunc`

``` purescript
depthFunc :: forall eff. Func -> Eff (webgl :: WebGl | eff) Unit
```

#### `defContextAttributes`

``` purescript
defContextAttributes :: ContextAttributes
```

#### `colorMask`

``` purescript
colorMask :: forall eff. GLboolean -> GLboolean -> GLboolean -> GLboolean -> Eff (webgl :: WebGl | eff) Unit
```

#### `clearStencil`

``` purescript
clearStencil :: forall eff. GLint -> Eff (webgl :: WebGl | eff) Unit
```

#### `clearDepth`

``` purescript
clearDepth :: forall eff. GLclampf -> Eff (webgl :: WebGl | eff) Unit
```

#### `clearColor`

``` purescript
clearColor :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit
```

#### `clear`

``` purescript
clear :: forall eff. Array Mask -> (Eff (webgl :: WebGl | eff) Unit)
```

#### `blendFuncSeparate`

``` purescript
blendFuncSeparate :: forall eff. BlendingFactor -> BlendingFactor -> BlendingFactor -> BlendingFactor -> (Eff (webgl :: WebGl | eff) Unit)
```

#### `blendFunc`

``` purescript
blendFunc :: forall eff. BlendingFactor -> BlendingFactor -> (Eff (webgl :: WebGl | eff) Unit)
```

#### `blendEquationSeparate`

``` purescript
blendEquationSeparate :: forall eff. BlendEquation -> BlendEquation -> (Eff (webgl :: WebGl | eff) Unit)
```

#### `blendEquation`

``` purescript
blendEquation :: forall eff. BlendEquation -> (Eff (webgl :: WebGl | eff) Unit)
```

#### `blendColor`

``` purescript
blendColor :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit
```

#### `bindBufAndSetVertexAttr`

``` purescript
bindBufAndSetVertexAttr :: forall a eff typ. Buffer a -> Attribute typ -> Eff (webgl :: WebGl | eff) Unit
```

#### `bindBuf`

``` purescript
bindBuf :: forall a eff. Buffer a -> Eff (webgl :: WebGl | eff) Unit
```

#### `bindAttribLocation`

``` purescript
bindAttribLocation :: forall eff. WebGLProg -> Int -> String -> Eff (webgl :: WebGl | eff) Unit
```

### Re-exported from Graphics.WebGLFramebuffer:

#### `WebGLRendBuf`

``` purescript
newtype WebGLRendBuf
  = WebGLRendBuf WebGLRenderbuffer
```

#### `WebGLBuf`

``` purescript
newtype WebGLBuf
  = WebGLBuf WebGLFramebuffer
```

#### `RenderbufferFormat`

``` purescript
data RenderbufferFormat
  = RGBA4
  | RGB565
  | RGB5_A1
  | DEPTH_COMPONENT16
```

#### `FrameBufferCode`

``` purescript
data FrameBufferCode
  = FRAMEBUFFER_COMPLETE
  | FRAMEBUFFER_INCOMPLETE_ATTACHMENT
  | FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
  | FRAMEBUFFER_INCOMPLETE_DIMENSIONS
  | FRAMEBUFFER_UNSUPPORTED
```

#### `AttachementPoint`

``` purescript
data AttachementPoint
  = COLOR_ATTACHMENT0
  | DEPTH_ATTACHMENT
  | STENCIL_ATTACHMENT
  | DEPTH_STENCIL_ATTACHMENT
```

#### `unbindRenderbuffer`

``` purescript
unbindRenderbuffer :: forall eff. EffWebGL eff Unit
```

#### `unbindFramebuffer`

``` purescript
unbindFramebuffer :: forall eff. EffWebGL eff Unit
```

#### `renderbufferStorage`

``` purescript
renderbufferStorage :: forall eff. RenderbufferFormat -> Int -> Int -> EffWebGL eff Unit
```

#### `readPixels`

``` purescript
readPixels :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> Uint8Array -> Eff (webgl :: WebGl | eff) Uint8Array
```

#### `framebufferTexture2D`

``` purescript
framebufferTexture2D :: forall eff. AttachementPoint -> TargetType -> WebGLTex -> EffWebGL eff Unit
```

#### `framebufferRenderbuffer`

``` purescript
framebufferRenderbuffer :: forall eff. AttachementPoint -> WebGLRendBuf -> EffWebGL eff Unit
```

#### `frameBufferCodeToConst`

``` purescript
frameBufferCodeToConst :: FrameBufferCode -> GLenum
```

#### `createRenderbuffer`

``` purescript
createRenderbuffer :: forall eff. EffWebGL eff WebGLRendBuf
```

#### `createFramebuffer`

``` purescript
createFramebuffer :: forall eff. EffWebGL eff WebGLBuf
```

#### `checkFramebufferStatus`

``` purescript
checkFramebufferStatus :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) GLenum
```

#### `bindRenderbuffer`

``` purescript
bindRenderbuffer :: forall eff. WebGLRendBuf -> EffWebGL eff Unit
```

#### `bindFramebuffer`

``` purescript
bindFramebuffer :: forall eff. WebGLBuf -> EffWebGL eff Unit
```

### Re-exported from Graphics.WebGLTexture:

#### `WebGLTex`

``` purescript
newtype WebGLTex
  = WebGLTex WebGLTexture
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

#### `TexTarget`

``` purescript
data TexTarget
  = TTEXTURE_2D
  | TTEXTURE_CUBE_MAP
```

#### `TexParName`

``` purescript
data TexParName
  = TEXTURE_MIN_FILTER
  | TEXTURE_MAG_FILTER
  | TEXTURE_WRAP_S
  | TEXTURE_WRAP_T
```

#### `TexFilterSpec`

``` purescript
data TexFilterSpec
  = NEAREST
  | LINEAR
  | MIPMAP
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

#### `SymbolicParameter`

``` purescript
data SymbolicParameter
  = PACK_ALIGNMENT
  | UNPACK_ALIGNMENT
  | UNPACK_FLIP_Y_WEBGL
  | UNPACK_PREMULTIPLY_ALPHA_WEBGL
  | UNPACK_COLORSPACE_CONVERSION_WEBGL
```

#### `InternalFormat`

``` purescript
data InternalFormat
  = IF_ALPHA
  | IF_LUMINANCE
  | IF_LUMINANCE_ALPHA
  | IF_RGB
  | IF_RGBA
```

#### `withTexture2D`

``` purescript
withTexture2D :: forall eff typ. WebGLTex -> Int -> Uniform typ -> Int -> EffWebGL eff Unit -> EffWebGL eff Unit
```

#### `unbindTexture`

``` purescript
unbindTexture :: forall eff. TargetType -> EffWebGL eff Unit
```

#### `texture2DFor`

``` purescript
texture2DFor :: forall a eff. String -> TexFilterSpec -> (WebGLTex -> EffWebGL eff a) -> EffWebGL eff Unit
```

#### `targetTypeToConst`

``` purescript
targetTypeToConst :: TargetType -> GLenum
```

#### `newTextureInit`

``` purescript
newTextureInit :: forall eff. Int -> Int -> TexFilterSpec -> EffWebGL eff WebGLTex
```

#### `newTexture`

``` purescript
newTexture :: forall eff. Int -> Int -> TexFilterSpec -> EffWebGL eff WebGLTex
```

#### `handleSubLoad2D`

``` purescript
handleSubLoad2D :: forall eff a. WebGLTex -> Int -> Int -> Int -> Int -> TexFilterSpec -> a -> EffWebGL eff Unit
```

#### `handleLoad2D`

``` purescript
handleLoad2D :: forall eff a. WebGLTex -> TexFilterSpec -> a -> EffWebGL eff Unit
```

#### `createTexture`

``` purescript
createTexture :: forall eff. Eff (webgl :: WebGl | eff) WebGLTex
```

#### `bindTexture`

``` purescript
bindTexture :: forall eff. TargetType -> WebGLTex -> EffWebGL eff Unit
```

#### `activeTexture`

``` purescript
activeTexture :: forall eff. Int -> Eff (webgl :: WebGl | eff) Unit
```

