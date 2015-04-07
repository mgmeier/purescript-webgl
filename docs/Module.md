# Module Documentation

## Module Control.Monad.Eff.WebGL


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



## Module Graphics.WebGL


#### `WebGLContext`

``` purescript
type WebGLContext = { canvasName :: String }
```


#### `ContextAttributes`

``` purescript
type ContextAttributes = { failIfMajorPerformanceCaveat :: Boolean, preferLowPowerToHighPerformance :: Boolean, preserveDrawingBuffer :: Boolean, premultipliedAlpha :: Boolean, antialias :: Boolean, stencil :: Boolean, depth :: Boolean, alpha :: Boolean }
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
  = Uniform { uType :: Number, uName :: String, uLocation :: WebGLUniformLocation }
```


#### `Attribute`

``` purescript
newtype Attribute typ
  = Attribute { aItemSize :: Number, aItemType :: Number, aName :: String, aLocation :: GLint }
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


#### `withShaders`

``` purescript
withShaders :: forall bindings eff a. Shaders (Object bindings) -> (String -> EffWebGL eff a) -> ({ webGLProgram :: WebGLProg | bindings } -> EffWebGL eff a) -> EffWebGL eff a
```


#### `Buffer`

``` purescript
type Buffer a = { bufferSize :: Number, bufferType :: Number, webGLBuffer :: WebGLBuffer }
```


#### `makeBufferFloat`

``` purescript
makeBufferFloat :: forall eff. [Number] -> Eff (webgl :: WebGl | eff) (Buffer T.Float32)
```


#### `makeBuffer`

``` purescript
makeBuffer :: forall a eff. BufferTarget -> ([Number] -> T.ArrayView a) -> [Number] -> Eff (webgl :: WebGl | eff) (Buffer a)
```


#### `setUniformFloats`

``` purescript
setUniformFloats :: forall eff typ. Uniform typ -> [Number] -> EffWebGL eff Unit
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
clear :: forall eff. [Mask] -> Eff (webgl :: WebGl | eff) Unit
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
drawElements :: forall a eff. Mode -> Number -> EffWebGL eff Unit
```


#### `enable`

``` purescript
enable :: forall eff. Capacity -> Eff (webgl :: WebGl | eff) Unit
```


#### `isEnabled`

``` purescript
isEnabled :: forall eff. Capacity -> Eff (webgl :: WebGl | eff) Boolean
```


#### `vertexPointer`

``` purescript
vertexPointer :: forall eff typ. Attribute typ -> EffWebGL eff Unit
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
getCanvasWidth :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Number
```


#### `getCanvasHeight`

``` purescript
getCanvasHeight :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Number
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


#### `requestAnimationFrame`

``` purescript
requestAnimationFrame :: forall a eff. Eff (webgl :: WebGl | eff) a -> Eff (webgl :: WebGl | eff) Unit
```



## Module Graphics.WebGLFramebuffer


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


#### `RenderbufferFormat`

``` purescript
data RenderbufferFormat
  = RGBA4 
  | RGB565 
  | RGB5_A1 
  | DEPTH_COMPONENT16 
```


#### `AttachementPoint`

``` purescript
data AttachementPoint
  = COLOR_ATTACHMENT0 
  | DEPTH_ATTACHMENT 
  | STENCIL_ATTACHMENT 
  | DEPTH_STENCIL_ATTACHMENT 
```


#### `createFramebuffer`

``` purescript
createFramebuffer :: forall eff. EffWebGL eff WebGLBuf
```


#### `bindFramebuffer`

``` purescript
bindFramebuffer :: forall eff. WebGLBuf -> EffWebGL eff Unit
```


#### `unbindFramebuffer`

``` purescript
unbindFramebuffer :: forall eff. EffWebGL eff Unit
```


#### `createRenderbuffer`

``` purescript
createRenderbuffer :: forall eff. EffWebGL eff WebGLRendBuf
```


#### `bindRenderbuffer`

``` purescript
bindRenderbuffer :: forall eff. WebGLRendBuf -> EffWebGL eff Unit
```


#### `unbindRenderbuffer`

``` purescript
unbindRenderbuffer :: forall eff. EffWebGL eff Unit
```


#### `renderbufferStorage`

``` purescript
renderbufferStorage :: forall eff. RenderbufferFormat -> Number -> Number -> EffWebGL eff Unit
```


#### `framebufferRenderbuffer`

``` purescript
framebufferRenderbuffer :: forall eff. AttachementPoint -> WebGLRendBuf -> EffWebGL eff Unit
```


#### `readPixels`

``` purescript
readPixels :: forall a eff. GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> ArrayView a -> Eff (webgl :: WebGl | eff) (ArrayView a)
```



## Module Graphics.WebGLRaw


#### `GLenum`

``` purescript
type GLenum = Number
```


#### `GLboolean`

``` purescript
type GLboolean = Boolean
```


#### `GLbitfield`

``` purescript
type GLbitfield = Number
```


#### `GLbyte`

``` purescript
type GLbyte = Number
```


#### `GLshort`

``` purescript
type GLshort = Number
```


#### `GLint`

``` purescript
type GLint = Number
```


#### `GLsizei`

``` purescript
type GLsizei = Number
```


#### `GLintptr`

``` purescript
type GLintptr = Number
```


#### `GLsizeiptr`

``` purescript
type GLsizeiptr = Number
```


#### `GLubyte`

``` purescript
type GLubyte = Number
```


#### `GLushort`

``` purescript
type GLushort = Number
```


#### `GLuint`

``` purescript
type GLuint = Number
```


#### `GLfloat`

``` purescript
type GLfloat = Number
```


#### `GLclampf`

``` purescript
type GLclampf = Number
```


#### `FloatArray`

``` purescript
type FloatArray = Float32Array
```


#### `WebGLContextAttributes`

``` purescript
data WebGLContextAttributes :: *
```

#### `WebGLProgram`

``` purescript
data WebGLProgram :: *
```


#### `WebGLShader`

``` purescript
data WebGLShader :: *
```


#### `WebGLBuffer`

``` purescript
data WebGLBuffer :: *
```


#### `WebGLFramebuffer`

``` purescript
data WebGLFramebuffer :: *
```


#### `WebGLRenderbuffer`

``` purescript
data WebGLRenderbuffer :: *
```


#### `WebGLTexture`

``` purescript
data WebGLTexture :: *
```


#### `WebGLActiveInfo`

``` purescript
data WebGLActiveInfo :: *
```


#### `WebGLUniformLocation`

``` purescript
data WebGLUniformLocation :: *
```


#### `ArrayBufferView`

``` purescript
data ArrayBufferView :: *
```


#### `ImageData`

``` purescript
data ImageData :: *
```


#### `HTMLImageElement`

``` purescript
data HTMLImageElement :: *
```


#### `HTMLVideoElement`

``` purescript
data HTMLVideoElement :: *
```


#### `_DEPTH_BUFFER_BIT`

``` purescript
_DEPTH_BUFFER_BIT :: Number
```

#### `_STENCIL_BUFFER_BIT`

``` purescript
_STENCIL_BUFFER_BIT :: Number
```


#### `_COLOR_BUFFER_BIT`

``` purescript
_COLOR_BUFFER_BIT :: Number
```


#### `_POINTS`

``` purescript
_POINTS :: Number
```


#### `_LINES`

``` purescript
_LINES :: Number
```


#### `_LINE_LOOP`

``` purescript
_LINE_LOOP :: Number
```


#### `_LINE_STRIP`

``` purescript
_LINE_STRIP :: Number
```


#### `_TRIANGLES`

``` purescript
_TRIANGLES :: Number
```


#### `_TRIANGLE_STRIP`

``` purescript
_TRIANGLE_STRIP :: Number
```


#### `_TRIANGLE_FAN`

``` purescript
_TRIANGLE_FAN :: Number
```


#### `_ZERO`

``` purescript
_ZERO :: Number
```


#### `_ONE`

``` purescript
_ONE :: Number
```


#### `_SRC_COLOR`

``` purescript
_SRC_COLOR :: Number
```


#### `_ONE_MINUS_SRC_COLOR`

``` purescript
_ONE_MINUS_SRC_COLOR :: Number
```


#### `_SRC_ALPHA`

``` purescript
_SRC_ALPHA :: Number
```


#### `_ONE_MINUS_SRC_ALPHA`

``` purescript
_ONE_MINUS_SRC_ALPHA :: Number
```


#### `_DST_ALPHA`

``` purescript
_DST_ALPHA :: Number
```


#### `_ONE_MINUS_DST_ALPHA`

``` purescript
_ONE_MINUS_DST_ALPHA :: Number
```


#### `_DST_COLOR`

``` purescript
_DST_COLOR :: Number
```


#### `_ONE_MINUS_DST_COLOR`

``` purescript
_ONE_MINUS_DST_COLOR :: Number
```


#### `_SRC_ALPHA_SATURATE`

``` purescript
_SRC_ALPHA_SATURATE :: Number
```


#### `_FUNC_ADD`

``` purescript
_FUNC_ADD :: Number
```


#### `_BLEND_EQUATION`

``` purescript
_BLEND_EQUATION :: Number
```


#### `_BLEND_EQUATION_RGB`

``` purescript
_BLEND_EQUATION_RGB :: Number
```


#### `_BLEND_EQUATION_ALPHA`

``` purescript
_BLEND_EQUATION_ALPHA :: Number
```


#### `_FUNC_SUBTRACT`

``` purescript
_FUNC_SUBTRACT :: Number
```


#### `_FUNC_REVERSE_SUBTRACT`

``` purescript
_FUNC_REVERSE_SUBTRACT :: Number
```


#### `_BLEND_DST_RGB`

``` purescript
_BLEND_DST_RGB :: Number
```


#### `_BLEND_SRC_RGB`

``` purescript
_BLEND_SRC_RGB :: Number
```


#### `_BLEND_DST_ALPHA`

``` purescript
_BLEND_DST_ALPHA :: Number
```


#### `_BLEND_SRC_ALPHA`

``` purescript
_BLEND_SRC_ALPHA :: Number
```


#### `_CONSTANT_COLOR`

``` purescript
_CONSTANT_COLOR :: Number
```


#### `_ONE_MINUS_CONSTANT_COLOR`

``` purescript
_ONE_MINUS_CONSTANT_COLOR :: Number
```


#### `_CONSTANT_ALPHA`

``` purescript
_CONSTANT_ALPHA :: Number
```


#### `_ONE_MINUS_CONSTANT_ALPHA`

``` purescript
_ONE_MINUS_CONSTANT_ALPHA :: Number
```


#### `_BLEND_COLOR`

``` purescript
_BLEND_COLOR :: Number
```


#### `_ARRAY_BUFFER`

``` purescript
_ARRAY_BUFFER :: Number
```


#### `_ELEMENT_ARRAY_BUFFER`

``` purescript
_ELEMENT_ARRAY_BUFFER :: Number
```


#### `_ARRAY_BUFFER_BINDING`

``` purescript
_ARRAY_BUFFER_BINDING :: Number
```


#### `_ELEMENT_ARRAY_BUFFER_BINDING`

``` purescript
_ELEMENT_ARRAY_BUFFER_BINDING :: Number
```


#### `_STREAM_DRAW`

``` purescript
_STREAM_DRAW :: Number
```


#### `_STATIC_DRAW`

``` purescript
_STATIC_DRAW :: Number
```


#### `_DYNAMIC_DRAW`

``` purescript
_DYNAMIC_DRAW :: Number
```


#### `_BUFFER_SIZE`

``` purescript
_BUFFER_SIZE :: Number
```


#### `_BUFFER_USAGE`

``` purescript
_BUFFER_USAGE :: Number
```


#### `_CURRENT_VERTEX_ATTRIB`

``` purescript
_CURRENT_VERTEX_ATTRIB :: Number
```


#### `_FRONT`

``` purescript
_FRONT :: Number
```


#### `_BACK`

``` purescript
_BACK :: Number
```


#### `_FRONT_AND_BACK`

``` purescript
_FRONT_AND_BACK :: Number
```


#### `_TEXTURE_2D`

``` purescript
_TEXTURE_2D :: Number
```


#### `_CULL_FACE`

``` purescript
_CULL_FACE :: Number
```


#### `_BLEND`

``` purescript
_BLEND :: Number
```


#### `_DITHER`

``` purescript
_DITHER :: Number
```


#### `_STENCIL_TEST`

``` purescript
_STENCIL_TEST :: Number
```


#### `_DEPTH_TEST`

``` purescript
_DEPTH_TEST :: Number
```


#### `_SCISSOR_TEST`

``` purescript
_SCISSOR_TEST :: Number
```


#### `_POLYGON_OFFSET_FILL`

``` purescript
_POLYGON_OFFSET_FILL :: Number
```


#### `_SAMPLE_ALPHA_TO_COVERAGE`

``` purescript
_SAMPLE_ALPHA_TO_COVERAGE :: Number
```


#### `_SAMPLE_COVERAGE`

``` purescript
_SAMPLE_COVERAGE :: Number
```


#### `_NO_ERROR`

``` purescript
_NO_ERROR :: Number
```


#### `_INVALID_ENUM`

``` purescript
_INVALID_ENUM :: Number
```


#### `_INVALID_VALUE`

``` purescript
_INVALID_VALUE :: Number
```


#### `_INVALID_OPERATION`

``` purescript
_INVALID_OPERATION :: Number
```


#### `_OUT_OF_MEMORY`

``` purescript
_OUT_OF_MEMORY :: Number
```


#### `_CW`

``` purescript
_CW :: Number
```


#### `_CCW`

``` purescript
_CCW :: Number
```


#### `_LINE_WIDTH`

``` purescript
_LINE_WIDTH :: Number
```


#### `_ALIASED_POINT_SIZE_RANGE`

``` purescript
_ALIASED_POINT_SIZE_RANGE :: Number
```


#### `_ALIASED_LINE_WIDTH_RANGE`

``` purescript
_ALIASED_LINE_WIDTH_RANGE :: Number
```


#### `_CULL_FACE_MODE`

``` purescript
_CULL_FACE_MODE :: Number
```


#### `_FRONT_FACE`

``` purescript
_FRONT_FACE :: Number
```


#### `_DEPTH_RANGE`

``` purescript
_DEPTH_RANGE :: Number
```


#### `_DEPTH_WRITEMASK`

``` purescript
_DEPTH_WRITEMASK :: Number
```


#### `_DEPTH_CLEAR_VALUE`

``` purescript
_DEPTH_CLEAR_VALUE :: Number
```


#### `_DEPTH_FUNC`

``` purescript
_DEPTH_FUNC :: Number
```


#### `_STENCIL_CLEAR_VALUE`

``` purescript
_STENCIL_CLEAR_VALUE :: Number
```


#### `_STENCIL_FUNC`

``` purescript
_STENCIL_FUNC :: Number
```


#### `_STENCIL_FAIL`

``` purescript
_STENCIL_FAIL :: Number
```


#### `_STENCIL_PASS_DEPTH_FAIL`

``` purescript
_STENCIL_PASS_DEPTH_FAIL :: Number
```


#### `_STENCIL_PASS_DEPTH_PASS`

``` purescript
_STENCIL_PASS_DEPTH_PASS :: Number
```


#### `_STENCIL_REF`

``` purescript
_STENCIL_REF :: Number
```


#### `_STENCIL_VALUE_MASK`

``` purescript
_STENCIL_VALUE_MASK :: Number
```


#### `_STENCIL_WRITEMASK`

``` purescript
_STENCIL_WRITEMASK :: Number
```


#### `_STENCIL_BACK_FUNC`

``` purescript
_STENCIL_BACK_FUNC :: Number
```


#### `_STENCIL_BACK_FAIL`

``` purescript
_STENCIL_BACK_FAIL :: Number
```


#### `_STENCIL_BACK_PASS_DEPTH_FAIL`

``` purescript
_STENCIL_BACK_PASS_DEPTH_FAIL :: Number
```


#### `_STENCIL_BACK_PASS_DEPTH_PASS`

``` purescript
_STENCIL_BACK_PASS_DEPTH_PASS :: Number
```


#### `_STENCIL_BACK_REF`

``` purescript
_STENCIL_BACK_REF :: Number
```


#### `_STENCIL_BACK_VALUE_MASK`

``` purescript
_STENCIL_BACK_VALUE_MASK :: Number
```


#### `_STENCIL_BACK_WRITEMASK`

``` purescript
_STENCIL_BACK_WRITEMASK :: Number
```


#### `_VIEWPORT`

``` purescript
_VIEWPORT :: Number
```


#### `_SCISSOR_BOX`

``` purescript
_SCISSOR_BOX :: Number
```


#### `_COLOR_CLEAR_VALUE`

``` purescript
_COLOR_CLEAR_VALUE :: Number
```


#### `_COLOR_WRITEMASK`

``` purescript
_COLOR_WRITEMASK :: Number
```


#### `_UNPACK_ALIGNMENT`

``` purescript
_UNPACK_ALIGNMENT :: Number
```


#### `_PACK_ALIGNMENT`

``` purescript
_PACK_ALIGNMENT :: Number
```


#### `_MAX_TEXTURE_SIZE`

``` purescript
_MAX_TEXTURE_SIZE :: Number
```


#### `_MAX_VIEWPORT_DIMS`

``` purescript
_MAX_VIEWPORT_DIMS :: Number
```


#### `_SUBPIXEL_BITS`

``` purescript
_SUBPIXEL_BITS :: Number
```


#### `_RED_BITS`

``` purescript
_RED_BITS :: Number
```


#### `_GREEN_BITS`

``` purescript
_GREEN_BITS :: Number
```


#### `_BLUE_BITS`

``` purescript
_BLUE_BITS :: Number
```


#### `_ALPHA_BITS`

``` purescript
_ALPHA_BITS :: Number
```


#### `_DEPTH_BITS`

``` purescript
_DEPTH_BITS :: Number
```


#### `_STENCIL_BITS`

``` purescript
_STENCIL_BITS :: Number
```


#### `_POLYGON_OFFSET_UNITS`

``` purescript
_POLYGON_OFFSET_UNITS :: Number
```


#### `_POLYGON_OFFSET_FACTOR`

``` purescript
_POLYGON_OFFSET_FACTOR :: Number
```


#### `_TEXTURE_BINDING_2D`

``` purescript
_TEXTURE_BINDING_2D :: Number
```


#### `_SAMPLE_BUFFERS`

``` purescript
_SAMPLE_BUFFERS :: Number
```


#### `_SAMPLES`

``` purescript
_SAMPLES :: Number
```


#### `_SAMPLE_COVERAGE_VALUE`

``` purescript
_SAMPLE_COVERAGE_VALUE :: Number
```


#### `_SAMPLE_COVERAGE_INVERT`

``` purescript
_SAMPLE_COVERAGE_INVERT :: Number
```


#### `_NUM_COMPRESSED_TEXTURE_FORMATS`

``` purescript
_NUM_COMPRESSED_TEXTURE_FORMATS :: Number
```


#### `_COMPRESSED_TEXTURE_FORMATS`

``` purescript
_COMPRESSED_TEXTURE_FORMATS :: Number
```


#### `_DONT_CARE`

``` purescript
_DONT_CARE :: Number
```


#### `_FASTEST`

``` purescript
_FASTEST :: Number
```


#### `_NICEST`

``` purescript
_NICEST :: Number
```


#### `_GENERATE_MIPMAP_HINT`

``` purescript
_GENERATE_MIPMAP_HINT :: Number
```


#### `_BYTE`

``` purescript
_BYTE :: Number
```


#### `_UNSIGNED_BYTE`

``` purescript
_UNSIGNED_BYTE :: Number
```


#### `_SHORT`

``` purescript
_SHORT :: Number
```


#### `_UNSIGNED_SHORT`

``` purescript
_UNSIGNED_SHORT :: Number
```


#### `_INT`

``` purescript
_INT :: Number
```


#### `_UNSIGNED_INT`

``` purescript
_UNSIGNED_INT :: Number
```


#### `_FLOAT`

``` purescript
_FLOAT :: Number
```


#### `_DEPTH_COMPONENT`

``` purescript
_DEPTH_COMPONENT :: Number
```


#### `_ALPHA`

``` purescript
_ALPHA :: Number
```


#### `_RGB`

``` purescript
_RGB :: Number
```


#### `_RGBA`

``` purescript
_RGBA :: Number
```


#### `_LUMINANCE`

``` purescript
_LUMINANCE :: Number
```


#### `_LUMINANCE_ALPHA`

``` purescript
_LUMINANCE_ALPHA :: Number
```


#### `_UNSIGNED_SHORT_4_4_4_4`

``` purescript
_UNSIGNED_SHORT_4_4_4_4 :: Number
```


#### `_UNSIGNED_SHORT_5_5_5_1`

``` purescript
_UNSIGNED_SHORT_5_5_5_1 :: Number
```


#### `_UNSIGNED_SHORT_5_6_5`

``` purescript
_UNSIGNED_SHORT_5_6_5 :: Number
```


#### `_FRAGMENT_SHADER`

``` purescript
_FRAGMENT_SHADER :: Number
```


#### `_VERTEX_SHADER`

``` purescript
_VERTEX_SHADER :: Number
```


#### `_MAX_VERTEX_ATTRIBS`

``` purescript
_MAX_VERTEX_ATTRIBS :: Number
```


#### `_MAX_VERTEX_UNIFORM_VECTORS`

``` purescript
_MAX_VERTEX_UNIFORM_VECTORS :: Number
```


#### `_MAX_VARYING_VECTORS`

``` purescript
_MAX_VARYING_VECTORS :: Number
```


#### `_MAX_COMBINED_TEXTURE_IMAGE_UNITS`

``` purescript
_MAX_COMBINED_TEXTURE_IMAGE_UNITS :: Number
```


#### `_MAX_VERTEX_TEXTURE_IMAGE_UNITS`

``` purescript
_MAX_VERTEX_TEXTURE_IMAGE_UNITS :: Number
```


#### `_MAX_TEXTURE_IMAGE_UNITS`

``` purescript
_MAX_TEXTURE_IMAGE_UNITS :: Number
```


#### `_MAX_FRAGMENT_UNIFORM_VECTORS`

``` purescript
_MAX_FRAGMENT_UNIFORM_VECTORS :: Number
```


#### `_SHADER_TYPE`

``` purescript
_SHADER_TYPE :: Number
```


#### `_DELETE_STATUS`

``` purescript
_DELETE_STATUS :: Number
```


#### `_LINK_STATUS`

``` purescript
_LINK_STATUS :: Number
```


#### `_VALIDATE_STATUS`

``` purescript
_VALIDATE_STATUS :: Number
```


#### `_ATTACHED_SHADERS`

``` purescript
_ATTACHED_SHADERS :: Number
```


#### `_ACTIVE_UNIFORMS`

``` purescript
_ACTIVE_UNIFORMS :: Number
```


#### `_ACTIVE_UNIFORM_MAX_LENGTH`

``` purescript
_ACTIVE_UNIFORM_MAX_LENGTH :: Number
```


#### `_ACTIVE_ATTRIBUTES`

``` purescript
_ACTIVE_ATTRIBUTES :: Number
```


#### `_ACTIVE_ATTRIBUTE_MAX_LENGTH`

``` purescript
_ACTIVE_ATTRIBUTE_MAX_LENGTH :: Number
```


#### `_SHADING_LANGUAGE_VERSION`

``` purescript
_SHADING_LANGUAGE_VERSION :: Number
```


#### `_CURRENT_PROGRAM`

``` purescript
_CURRENT_PROGRAM :: Number
```


#### `_NEVER`

``` purescript
_NEVER :: Number
```


#### `_LESS`

``` purescript
_LESS :: Number
```


#### `_EQUAL`

``` purescript
_EQUAL :: Number
```


#### `_LEQUAL`

``` purescript
_LEQUAL :: Number
```


#### `_GREATER`

``` purescript
_GREATER :: Number
```


#### `_NOTEQUAL`

``` purescript
_NOTEQUAL :: Number
```


#### `_GEQUAL`

``` purescript
_GEQUAL :: Number
```


#### `_ALWAYS`

``` purescript
_ALWAYS :: Number
```


#### `_KEEP`

``` purescript
_KEEP :: Number
```


#### `_REPLACE`

``` purescript
_REPLACE :: Number
```


#### `_INCR`

``` purescript
_INCR :: Number
```


#### `_DECR`

``` purescript
_DECR :: Number
```


#### `_INVERT`

``` purescript
_INVERT :: Number
```


#### `_INCR_WRAP`

``` purescript
_INCR_WRAP :: Number
```


#### `_DECR_WRAP`

``` purescript
_DECR_WRAP :: Number
```


#### `_VENDOR`

``` purescript
_VENDOR :: Number
```


#### `_RENDERER`

``` purescript
_RENDERER :: Number
```


#### `_VERSION`

``` purescript
_VERSION :: Number
```


#### `_NEAREST`

``` purescript
_NEAREST :: Number
```


#### `_LINEAR`

``` purescript
_LINEAR :: Number
```


#### `_NEAREST_MIPMAP_NEAREST`

``` purescript
_NEAREST_MIPMAP_NEAREST :: Number
```


#### `_LINEAR_MIPMAP_NEAREST`

``` purescript
_LINEAR_MIPMAP_NEAREST :: Number
```


#### `_NEAREST_MIPMAP_LINEAR`

``` purescript
_NEAREST_MIPMAP_LINEAR :: Number
```


#### `_LINEAR_MIPMAP_LINEAR`

``` purescript
_LINEAR_MIPMAP_LINEAR :: Number
```


#### `_TEXTURE_MAG_FILTER`

``` purescript
_TEXTURE_MAG_FILTER :: Number
```


#### `_TEXTURE_MIN_FILTER`

``` purescript
_TEXTURE_MIN_FILTER :: Number
```


#### `_TEXTURE_WRAP_S`

``` purescript
_TEXTURE_WRAP_S :: Number
```


#### `_TEXTURE_WRAP_T`

``` purescript
_TEXTURE_WRAP_T :: Number
```


#### `_TEXTURE`

``` purescript
_TEXTURE :: Number
```


#### `_TEXTURE_CUBE_MAP`

``` purescript
_TEXTURE_CUBE_MAP :: Number
```


#### `_TEXTURE_BINDING_CUBE_MAP`

``` purescript
_TEXTURE_BINDING_CUBE_MAP :: Number
```


#### `_TEXTURE_CUBE_MAP_POSITIVE_X`

``` purescript
_TEXTURE_CUBE_MAP_POSITIVE_X :: Number
```


#### `_TEXTURE_CUBE_MAP_NEGATIVE_X`

``` purescript
_TEXTURE_CUBE_MAP_NEGATIVE_X :: Number
```


#### `_TEXTURE_CUBE_MAP_POSITIVE_Y`

``` purescript
_TEXTURE_CUBE_MAP_POSITIVE_Y :: Number
```


#### `_TEXTURE_CUBE_MAP_NEGATIVE_Y`

``` purescript
_TEXTURE_CUBE_MAP_NEGATIVE_Y :: Number
```


#### `_TEXTURE_CUBE_MAP_POSITIVE_Z`

``` purescript
_TEXTURE_CUBE_MAP_POSITIVE_Z :: Number
```


#### `_TEXTURE_CUBE_MAP_NEGATIVE_Z`

``` purescript
_TEXTURE_CUBE_MAP_NEGATIVE_Z :: Number
```


#### `_MAX_CUBE_MAP_TEXTURE_SIZE`

``` purescript
_MAX_CUBE_MAP_TEXTURE_SIZE :: Number
```


#### `_TEXTURE0`

``` purescript
_TEXTURE0 :: Number
```


#### `_TEXTURE1`

``` purescript
_TEXTURE1 :: Number
```


#### `_TEXTURE2`

``` purescript
_TEXTURE2 :: Number
```


#### `_TEXTURE3`

``` purescript
_TEXTURE3 :: Number
```


#### `_TEXTURE4`

``` purescript
_TEXTURE4 :: Number
```


#### `_TEXTURE5`

``` purescript
_TEXTURE5 :: Number
```


#### `_TEXTURE6`

``` purescript
_TEXTURE6 :: Number
```


#### `_TEXTURE7`

``` purescript
_TEXTURE7 :: Number
```


#### `_TEXTURE8`

``` purescript
_TEXTURE8 :: Number
```


#### `_TEXTURE9`

``` purescript
_TEXTURE9 :: Number
```


#### `_TEXTURE10`

``` purescript
_TEXTURE10 :: Number
```


#### `_TEXTURE11`

``` purescript
_TEXTURE11 :: Number
```


#### `_TEXTURE12`

``` purescript
_TEXTURE12 :: Number
```


#### `_TEXTURE13`

``` purescript
_TEXTURE13 :: Number
```


#### `_TEXTURE14`

``` purescript
_TEXTURE14 :: Number
```


#### `_TEXTURE15`

``` purescript
_TEXTURE15 :: Number
```


#### `_TEXTURE16`

``` purescript
_TEXTURE16 :: Number
```


#### `_TEXTURE17`

``` purescript
_TEXTURE17 :: Number
```


#### `_TEXTURE18`

``` purescript
_TEXTURE18 :: Number
```


#### `_TEXTURE19`

``` purescript
_TEXTURE19 :: Number
```


#### `_TEXTURE20`

``` purescript
_TEXTURE20 :: Number
```


#### `_TEXTURE21`

``` purescript
_TEXTURE21 :: Number
```


#### `_TEXTURE22`

``` purescript
_TEXTURE22 :: Number
```


#### `_TEXTURE23`

``` purescript
_TEXTURE23 :: Number
```


#### `_TEXTURE24`

``` purescript
_TEXTURE24 :: Number
```


#### `_TEXTURE25`

``` purescript
_TEXTURE25 :: Number
```


#### `_TEXTURE26`

``` purescript
_TEXTURE26 :: Number
```


#### `_TEXTURE27`

``` purescript
_TEXTURE27 :: Number
```


#### `_TEXTURE28`

``` purescript
_TEXTURE28 :: Number
```


#### `_TEXTURE29`

``` purescript
_TEXTURE29 :: Number
```


#### `_TEXTURE30`

``` purescript
_TEXTURE30 :: Number
```


#### `_TEXTURE31`

``` purescript
_TEXTURE31 :: Number
```


#### `_ACTIVE_TEXTURE`

``` purescript
_ACTIVE_TEXTURE :: Number
```


#### `_REPEAT`

``` purescript
_REPEAT :: Number
```


#### `_CLAMP_TO_EDGE`

``` purescript
_CLAMP_TO_EDGE :: Number
```


#### `_MIRRORED_REPEAT`

``` purescript
_MIRRORED_REPEAT :: Number
```


#### `_FLOAT_VEC2`

``` purescript
_FLOAT_VEC2 :: Number
```


#### `_FLOAT_VEC3`

``` purescript
_FLOAT_VEC3 :: Number
```


#### `_FLOAT_VEC4`

``` purescript
_FLOAT_VEC4 :: Number
```


#### `_INT_VEC2`

``` purescript
_INT_VEC2 :: Number
```


#### `_INT_VEC3`

``` purescript
_INT_VEC3 :: Number
```


#### `_INT_VEC4`

``` purescript
_INT_VEC4 :: Number
```


#### `_BOOL`

``` purescript
_BOOL :: Number
```


#### `_BOOL_VEC2`

``` purescript
_BOOL_VEC2 :: Number
```


#### `_BOOL_VEC3`

``` purescript
_BOOL_VEC3 :: Number
```


#### `_BOOL_VEC4`

``` purescript
_BOOL_VEC4 :: Number
```


#### `_FLOAT_MAT2`

``` purescript
_FLOAT_MAT2 :: Number
```


#### `_FLOAT_MAT3`

``` purescript
_FLOAT_MAT3 :: Number
```


#### `_FLOAT_MAT4`

``` purescript
_FLOAT_MAT4 :: Number
```


#### `_SAMPLER_2D`

``` purescript
_SAMPLER_2D :: Number
```


#### `_SAMPLER_CUBE`

``` purescript
_SAMPLER_CUBE :: Number
```


#### `_VERTEX_ATTRIB_ARRAY_ENABLED`

``` purescript
_VERTEX_ATTRIB_ARRAY_ENABLED :: Number
```


#### `_VERTEX_ATTRIB_ARRAY_SIZE`

``` purescript
_VERTEX_ATTRIB_ARRAY_SIZE :: Number
```


#### `_VERTEX_ATTRIB_ARRAY_STRIDE`

``` purescript
_VERTEX_ATTRIB_ARRAY_STRIDE :: Number
```


#### `_VERTEX_ATTRIB_ARRAY_TYPE`

``` purescript
_VERTEX_ATTRIB_ARRAY_TYPE :: Number
```


#### `_VERTEX_ATTRIB_ARRAY_NORMALIZED`

``` purescript
_VERTEX_ATTRIB_ARRAY_NORMALIZED :: Number
```


#### `_VERTEX_ATTRIB_ARRAY_POINTER`

``` purescript
_VERTEX_ATTRIB_ARRAY_POINTER :: Number
```


#### `_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING`

``` purescript
_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: Number
```


#### `_COMPILE_STATUS`

``` purescript
_COMPILE_STATUS :: Number
```


#### `_INFO_LOG_LENGTH`

``` purescript
_INFO_LOG_LENGTH :: Number
```


#### `_SHADER_SOURCE_LENGTH`

``` purescript
_SHADER_SOURCE_LENGTH :: Number
```


#### `_LOW_FLOAT`

``` purescript
_LOW_FLOAT :: Number
```


#### `_MEDIUM_FLOAT`

``` purescript
_MEDIUM_FLOAT :: Number
```


#### `_HIGH_FLOAT`

``` purescript
_HIGH_FLOAT :: Number
```


#### `_LOW_INT`

``` purescript
_LOW_INT :: Number
```


#### `_MEDIUM_INT`

``` purescript
_MEDIUM_INT :: Number
```


#### `_HIGH_INT`

``` purescript
_HIGH_INT :: Number
```


#### `_FRAMEBUFFER`

``` purescript
_FRAMEBUFFER :: Number
```


#### `_RENDERBUFFER`

``` purescript
_RENDERBUFFER :: Number
```


#### `_RGBA4`

``` purescript
_RGBA4 :: Number
```


#### `_RGB5_A1`

``` purescript
_RGB5_A1 :: Number
```


#### `_RGB565`

``` purescript
_RGB565 :: Number
```


#### `_DEPTH_COMPONENT16`

``` purescript
_DEPTH_COMPONENT16 :: Number
```


#### `_STENCIL_INDEX`

``` purescript
_STENCIL_INDEX :: Number
```


#### `_STENCIL_INDEX8`

``` purescript
_STENCIL_INDEX8 :: Number
```


#### `_DEPTH_STENCIL`

``` purescript
_DEPTH_STENCIL :: Number
```


#### `_RENDERBUFFER_WIDTH`

``` purescript
_RENDERBUFFER_WIDTH :: Number
```


#### `_RENDERBUFFER_HEIGHT`

``` purescript
_RENDERBUFFER_HEIGHT :: Number
```


#### `_RENDERBUFFER_INTERNAL_FORMAT`

``` purescript
_RENDERBUFFER_INTERNAL_FORMAT :: Number
```


#### `_RENDERBUFFER_RED_SIZE`

``` purescript
_RENDERBUFFER_RED_SIZE :: Number
```


#### `_RENDERBUFFER_GREEN_SIZE`

``` purescript
_RENDERBUFFER_GREEN_SIZE :: Number
```


#### `_RENDERBUFFER_BLUE_SIZE`

``` purescript
_RENDERBUFFER_BLUE_SIZE :: Number
```


#### `_RENDERBUFFER_ALPHA_SIZE`

``` purescript
_RENDERBUFFER_ALPHA_SIZE :: Number
```


#### `_RENDERBUFFER_DEPTH_SIZE`

``` purescript
_RENDERBUFFER_DEPTH_SIZE :: Number
```


#### `_RENDERBUFFER_STENCIL_SIZE`

``` purescript
_RENDERBUFFER_STENCIL_SIZE :: Number
```


#### `_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE`

``` purescript
_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: Number
```


#### `_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME`

``` purescript
_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: Number
```


#### `_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL`

``` purescript
_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: Number
```


#### `_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE`

``` purescript
_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: Number
```


#### `_COLOR_ATTACHMENT0`

``` purescript
_COLOR_ATTACHMENT0 :: Number
```


#### `_DEPTH_ATTACHMENT`

``` purescript
_DEPTH_ATTACHMENT :: Number
```


#### `_STENCIL_ATTACHMENT`

``` purescript
_STENCIL_ATTACHMENT :: Number
```


#### `_DEPTH_STENCIL_ATTACHMENT`

``` purescript
_DEPTH_STENCIL_ATTACHMENT :: Number
```


#### `_NONE`

``` purescript
_NONE :: Number
```


#### `_FRAMEBUFFER_COMPLETE`

``` purescript
_FRAMEBUFFER_COMPLETE :: Number
```


#### `_FRAMEBUFFER_INCOMPLETE_ATTACHMENT`

``` purescript
_FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: Number
```


#### `_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT`

``` purescript
_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: Number
```


#### `_FRAMEBUFFER_INCOMPLETE_DIMENSIONS`

``` purescript
_FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: Number
```


#### `_FRAMEBUFFER_UNSUPPORTED`

``` purescript
_FRAMEBUFFER_UNSUPPORTED :: Number
```


#### `_FRAMEBUFFER_BINDING`

``` purescript
_FRAMEBUFFER_BINDING :: Number
```


#### `_RENDERBUFFER_BINDING`

``` purescript
_RENDERBUFFER_BINDING :: Number
```


#### `_MAX_RENDERBUFFER_SIZE`

``` purescript
_MAX_RENDERBUFFER_SIZE :: Number
```


#### `_INVALID_FRAMEBUFFER_OPERATION`

``` purescript
_INVALID_FRAMEBUFFER_OPERATION :: Number
```


#### `_UNPACK_FLIP_Y_WEBGL`

``` purescript
_UNPACK_FLIP_Y_WEBGL :: Number
```


#### `_UNPACK_PREMULTIPLY_ALPHA_WEBGL`

``` purescript
_UNPACK_PREMULTIPLY_ALPHA_WEBGL :: Number
```


#### `_CONTEXT_LOST_WEBGL`

``` purescript
_CONTEXT_LOST_WEBGL :: Number
```


#### `_UNPACK_COLORSPACE_CONVERSION_WEBGL`

``` purescript
_UNPACK_COLORSPACE_CONVERSION_WEBGL :: Number
```


#### `_BROWSER_DEFAULT_WEBGL`

``` purescript
_BROWSER_DEFAULT_WEBGL :: Number
```


#### `getContextAttributes_`

``` purescript
getContextAttributes_ :: forall eff. Eff (webgl :: WebGl | eff) WebGLContextAttributes
```

#### `isContextLost_`

``` purescript
isContextLost_ :: forall eff. Eff (webgl :: WebGl | eff) Boolean
```


#### `getSupportedExtensions_`

``` purescript
getSupportedExtensions_ :: forall eff. Eff (webgl :: WebGl | eff) String
```


#### `getExtension_`

``` purescript
getExtension_ :: forall eff ret. String -> Eff (webgl :: WebGl | eff) ret
```


#### `activeTexture_`

``` purescript
activeTexture_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `attachShader_`

``` purescript
attachShader_ :: forall eff. WebGLProgram -> WebGLShader -> Eff (webgl :: WebGl | eff) Unit
```


#### `bindAttribLocation_`

``` purescript
bindAttribLocation_ :: forall eff. WebGLProgram -> GLuint -> String -> Eff (webgl :: WebGl | eff) Unit
```


#### `bindBuffer_`

``` purescript
bindBuffer_ :: forall eff. GLenum -> WebGLBuffer -> Eff (webgl :: WebGl | eff) Unit
```


#### `bindFramebuffer_`

``` purescript
bindFramebuffer_ :: forall eff. GLenum -> WebGLFramebuffer -> Eff (webgl :: WebGl | eff) Unit
```


#### `bindRenderbuffer_`

``` purescript
bindRenderbuffer_ :: forall eff. GLenum -> WebGLRenderbuffer -> Eff (webgl :: WebGl | eff) Unit
```


#### `bindTexture_`

``` purescript
bindTexture_ :: forall eff. GLenum -> WebGLTexture -> Eff (webgl :: WebGl | eff) Unit
```


#### `blendColor_`

``` purescript
blendColor_ :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit
```


#### `blendEquation_`

``` purescript
blendEquation_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `blendEquationSeparate_`

``` purescript
blendEquationSeparate_ :: forall eff. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `blendFunc_`

``` purescript
blendFunc_ :: forall eff. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `blendFuncSeparate_`

``` purescript
blendFuncSeparate_ :: forall eff. GLenum -> GLenum -> GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `bufferData_`

``` purescript
bufferData_ :: forall eff. GLenum -> Float32Array -> GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `bufferSubData_`

``` purescript
bufferSubData_ :: forall eff. GLenum -> GLintptr -> Float32Array -> Eff (webgl :: WebGl | eff) Unit
```


#### `checkFramebufferStatus_`

``` purescript
checkFramebufferStatus_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) GLenum
```


#### `clear_`

``` purescript
clear_ :: forall eff. GLbitfield -> Eff (webgl :: WebGl | eff) Unit
```


#### `clearColor_`

``` purescript
clearColor_ :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit
```


#### `clearDepth_`

``` purescript
clearDepth_ :: forall eff. GLclampf -> Eff (webgl :: WebGl | eff) Unit
```


#### `clearStencil_`

``` purescript
clearStencil_ :: forall eff. GLint -> Eff (webgl :: WebGl | eff) Unit
```


#### `colorMask_`

``` purescript
colorMask_ :: forall eff. GLboolean -> GLboolean -> GLboolean -> GLboolean -> Eff (webgl :: WebGl | eff) Unit
```


#### `compileShader_`

``` purescript
compileShader_ :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) Unit
```


#### `copyTexImage2D_`

``` purescript
copyTexImage2D_ :: forall eff. GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> Eff (webgl :: WebGl | eff) Unit
```


#### `copyTexSubImage2D_`

``` purescript
copyTexSubImage2D_ :: forall eff. GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit
```


#### `createBuffer_`

``` purescript
createBuffer_ :: forall eff. Eff (webgl :: WebGl | eff) WebGLBuffer
```


#### `createFramebuffer_`

``` purescript
createFramebuffer_ :: forall eff. Eff (webgl :: WebGl | eff) WebGLFramebuffer
```


#### `createProgram_`

``` purescript
createProgram_ :: forall eff. Eff (webgl :: WebGl | eff) WebGLProgram
```


#### `createRenderbuffer_`

``` purescript
createRenderbuffer_ :: forall eff. Eff (webgl :: WebGl | eff) WebGLRenderbuffer
```


#### `createShader_`

``` purescript
createShader_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) WebGLShader
```


#### `createTexture_`

``` purescript
createTexture_ :: forall eff. Eff (webgl :: WebGl | eff) WebGLTexture
```


#### `cullFace_`

``` purescript
cullFace_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `deleteBuffer_`

``` purescript
deleteBuffer_ :: forall eff. WebGLBuffer -> Eff (webgl :: WebGl | eff) Unit
```


#### `deleteFramebuffer_`

``` purescript
deleteFramebuffer_ :: forall eff. WebGLFramebuffer -> Eff (webgl :: WebGl | eff) Unit
```


#### `deleteProgram_`

``` purescript
deleteProgram_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) Unit
```


#### `deleteRenderbuffer_`

``` purescript
deleteRenderbuffer_ :: forall eff. WebGLRenderbuffer -> Eff (webgl :: WebGl | eff) Unit
```


#### `deleteShader_`

``` purescript
deleteShader_ :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) Unit
```


#### `deleteTexture_`

``` purescript
deleteTexture_ :: forall eff. WebGLTexture -> Eff (webgl :: WebGl | eff) Unit
```


#### `depthFunc_`

``` purescript
depthFunc_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `depthMask_`

``` purescript
depthMask_ :: forall eff. GLboolean -> Eff (webgl :: WebGl | eff) Unit
```


#### `depthRange_`

``` purescript
depthRange_ :: forall eff. GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit
```


#### `detachShader_`

``` purescript
detachShader_ :: forall eff. WebGLProgram -> WebGLShader -> Eff (webgl :: WebGl | eff) Unit
```


#### `disable_`

``` purescript
disable_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `disableVertexAttribArray_`

``` purescript
disableVertexAttribArray_ :: forall eff. GLuint -> Eff (webgl :: WebGl | eff) Unit
```


#### `drawArrays_`

``` purescript
drawArrays_ :: forall eff. GLenum -> GLint -> GLsizei -> Eff (webgl :: WebGl | eff) Unit
```


#### `drawElements_`

``` purescript
drawElements_ :: forall eff. GLenum -> GLsizei -> GLenum -> GLintptr -> Eff (webgl :: WebGl | eff) Unit
```


#### `enable_`

``` purescript
enable_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `enableVertexAttribArray_`

``` purescript
enableVertexAttribArray_ :: forall eff. GLuint -> Eff (webgl :: WebGl | eff) Unit
```


#### `finish_`

``` purescript
finish_ :: forall eff. Eff (webgl :: WebGl | eff) Unit
```


#### `flush_`

``` purescript
flush_ :: forall eff. Eff (webgl :: WebGl | eff) Unit
```


#### `framebufferRenderbuffer_`

``` purescript
framebufferRenderbuffer_ :: forall eff. GLenum -> GLenum -> GLenum -> WebGLRenderbuffer -> Eff (webgl :: WebGl | eff) Unit
```


#### `framebufferTexture2D_`

``` purescript
framebufferTexture2D_ :: forall eff. GLenum -> GLenum -> GLenum -> WebGLTexture -> GLint -> Eff (webgl :: WebGl | eff) Unit
```


#### `frontFace_`

``` purescript
frontFace_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `generateMipmap_`

``` purescript
generateMipmap_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `getActiveAttrib_`

``` purescript
getActiveAttrib_ :: forall eff. WebGLProgram -> GLuint -> Eff (webgl :: WebGl | eff) WebGLActiveInfo
```


#### `getActiveUniform_`

``` purescript
getActiveUniform_ :: forall eff. WebGLProgram -> GLuint -> Eff (webgl :: WebGl | eff) WebGLActiveInfo
```


#### `getAttachedShaders_`

``` purescript
getAttachedShaders_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) WebGLShader
```


#### `getAttribLocation_`

``` purescript
getAttribLocation_ :: forall eff. WebGLProgram -> String -> Eff (webgl :: WebGl | eff) GLint
```


#### `getParameter_`

``` purescript
getParameter_ :: forall eff ret. GLenum -> Eff (webgl :: WebGl | eff) ret
```


#### `getBufferParameter_`

``` purescript
getBufferParameter_ :: forall eff ret. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) ret
```


#### `getError_`

``` purescript
getError_ :: forall eff. Eff (webgl :: WebGl | eff) GLenum
```


#### `getFramebufferAttachmentParameter_`

``` purescript
getFramebufferAttachmentParameter_ :: forall eff ret. GLenum -> GLenum -> GLenum -> Eff (webgl :: WebGl | eff) ret
```


#### `getProgramParameter_`

``` purescript
getProgramParameter_ :: forall eff ret. WebGLProgram -> GLenum -> Eff (webgl :: WebGl | eff) ret
```


#### `getProgramInfoLog_`

``` purescript
getProgramInfoLog_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) String
```


#### `getRenderbufferParameter_`

``` purescript
getRenderbufferParameter_ :: forall eff ret. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) ret
```


#### `getShaderParameter_`

``` purescript
getShaderParameter_ :: forall eff ret. WebGLShader -> GLenum -> Eff (webgl :: WebGl | eff) ret
```


#### `getShaderInfoLog_`

``` purescript
getShaderInfoLog_ :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) String
```


#### `getShaderSource_`

``` purescript
getShaderSource_ :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) String
```


#### `getTexParameter_`

``` purescript
getTexParameter_ :: forall eff ret. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) ret
```


#### `getUniform_`

``` purescript
getUniform_ :: forall eff ret. WebGLProgram -> WebGLUniformLocation -> Eff (webgl :: WebGl | eff) ret
```


#### `getUniformLocation_`

``` purescript
getUniformLocation_ :: forall eff. WebGLProgram -> String -> Eff (webgl :: WebGl | eff) WebGLUniformLocation
```


#### `getVertexAttrib_`

``` purescript
getVertexAttrib_ :: forall eff ret. GLuint -> GLenum -> Eff (webgl :: WebGl | eff) ret
```


#### `getVertexAttribOffset_`

``` purescript
getVertexAttribOffset_ :: forall eff. GLuint -> GLenum -> Eff (webgl :: WebGl | eff) GLsizeiptr
```


#### `hint_`

``` purescript
hint_ :: forall eff. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `isBuffer_`

``` purescript
isBuffer_ :: forall eff. WebGLBuffer -> Eff (webgl :: WebGl | eff) GLboolean
```


#### `isEnabled_`

``` purescript
isEnabled_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) GLboolean
```


#### `isFramebuffer_`

``` purescript
isFramebuffer_ :: forall eff. WebGLFramebuffer -> Eff (webgl :: WebGl | eff) GLboolean
```


#### `isProgram_`

``` purescript
isProgram_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) GLboolean
```


#### `isRenderbuffer_`

``` purescript
isRenderbuffer_ :: forall eff. WebGLRenderbuffer -> Eff (webgl :: WebGl | eff) GLboolean
```


#### `isShader_`

``` purescript
isShader_ :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) GLboolean
```


#### `isTexture_`

``` purescript
isTexture_ :: forall eff. WebGLTexture -> Eff (webgl :: WebGl | eff) GLboolean
```


#### `lineWidth_`

``` purescript
lineWidth_ :: forall eff. GLfloat -> Eff (webgl :: WebGl | eff) Unit
```


#### `linkProgram_`

``` purescript
linkProgram_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) Unit
```


#### `pixelStorei_`

``` purescript
pixelStorei_ :: forall eff. GLenum -> GLint -> Eff (webgl :: WebGl | eff) Unit
```


#### `polygonOffset_`

``` purescript
polygonOffset_ :: forall eff. GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit
```


#### `readPixels_`

``` purescript
readPixels_ :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> ArrayBufferView -> Eff (webgl :: WebGl | eff) Unit
```


#### `renderbufferStorage_`

``` purescript
renderbufferStorage_ :: forall eff. GLenum -> GLenum -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit
```


#### `sampleCoverage_`

``` purescript
sampleCoverage_ :: forall eff. GLclampf -> GLboolean -> Eff (webgl :: WebGl | eff) Unit
```


#### `scissor_`

``` purescript
scissor_ :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit
```


#### `shaderSource_`

``` purescript
shaderSource_ :: forall eff. WebGLShader -> String -> Eff (webgl :: WebGl | eff) Unit
```


#### `stencilFunc_`

``` purescript
stencilFunc_ :: forall eff. GLenum -> GLint -> GLuint -> Eff (webgl :: WebGl | eff) Unit
```


#### `stencilFuncSeparate_`

``` purescript
stencilFuncSeparate_ :: forall eff. GLenum -> GLenum -> GLint -> GLuint -> Eff (webgl :: WebGl | eff) Unit
```


#### `stencilMask_`

``` purescript
stencilMask_ :: forall eff. GLuint -> Eff (webgl :: WebGl | eff) Unit
```


#### `stencilMaskSeparate_`

``` purescript
stencilMaskSeparate_ :: forall eff. GLenum -> GLuint -> Eff (webgl :: WebGl | eff) Unit
```


#### `stencilOp_`

``` purescript
stencilOp_ :: forall eff. GLenum -> GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `stencilOpSeparate_`

``` purescript
stencilOpSeparate_ :: forall eff. GLenum -> GLenum -> GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit
```


#### `texImage2D_`

``` purescript
texImage2D_ :: forall eff. GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> ArrayBufferView -> Eff (webgl :: WebGl | eff) Unit
```


#### `texParameterf_`

``` purescript
texParameterf_ :: forall eff. GLenum -> GLenum -> GLfloat -> Eff (webgl :: WebGl | eff) Unit
```


#### `texParameteri_`

``` purescript
texParameteri_ :: forall eff. GLenum -> GLenum -> GLint -> Eff (webgl :: WebGl | eff) Unit
```


#### `texSubImage2D_`

``` purescript
texSubImage2D_ :: forall eff. GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> ArrayBufferView -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform1f_`

``` purescript
uniform1f_ :: forall eff. WebGLUniformLocation -> GLfloat -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform1fv_`

``` purescript
uniform1fv_ :: forall eff. WebGLUniformLocation -> FloatArray -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform1i_`

``` purescript
uniform1i_ :: forall eff. WebGLUniformLocation -> GLint -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform1iv_`

``` purescript
uniform1iv_ :: forall eff. WebGLUniformLocation -> Int32Array -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform2f_`

``` purescript
uniform2f_ :: forall eff. WebGLUniformLocation -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform2fv_`

``` purescript
uniform2fv_ :: forall eff. WebGLUniformLocation -> FloatArray -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform2i_`

``` purescript
uniform2i_ :: forall eff. WebGLUniformLocation -> GLint -> GLint -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform2iv_`

``` purescript
uniform2iv_ :: forall eff. WebGLUniformLocation -> Int32Array -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform3f_`

``` purescript
uniform3f_ :: forall eff. WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform3fv_`

``` purescript
uniform3fv_ :: forall eff. WebGLUniformLocation -> FloatArray -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform3i_`

``` purescript
uniform3i_ :: forall eff. WebGLUniformLocation -> GLint -> GLint -> GLint -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform3iv_`

``` purescript
uniform3iv_ :: forall eff. WebGLUniformLocation -> Int32Array -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform4f_`

``` purescript
uniform4f_ :: forall eff. WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform4fv_`

``` purescript
uniform4fv_ :: forall eff. WebGLUniformLocation -> FloatArray -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform4i_`

``` purescript
uniform4i_ :: forall eff. WebGLUniformLocation -> GLint -> GLint -> GLint -> GLint -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniform4iv_`

``` purescript
uniform4iv_ :: forall eff. WebGLUniformLocation -> Int32Array -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniformMatrix2fv_`

``` purescript
uniformMatrix2fv_ :: forall eff. WebGLUniformLocation -> GLboolean -> FloatArray -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniformMatrix3fv_`

``` purescript
uniformMatrix3fv_ :: forall eff. WebGLUniformLocation -> GLboolean -> FloatArray -> Eff (webgl :: WebGl | eff) Unit
```


#### `uniformMatrix4fv_`

``` purescript
uniformMatrix4fv_ :: forall eff. WebGLUniformLocation -> GLboolean -> FloatArray -> Eff (webgl :: WebGl | eff) Unit
```


#### `useProgram_`

``` purescript
useProgram_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) Unit
```


#### `validateProgram_`

``` purescript
validateProgram_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) Unit
```


#### `vertexAttrib1f_`

``` purescript
vertexAttrib1f_ :: forall eff. GLuint -> GLfloat -> Eff (webgl :: WebGl | eff) Unit
```


#### `vertexAttrib1fv_`

``` purescript
vertexAttrib1fv_ :: forall eff. GLuint -> FloatArray -> Eff (webgl :: WebGl | eff) Unit
```


#### `vertexAttrib2f_`

``` purescript
vertexAttrib2f_ :: forall eff. GLuint -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit
```


#### `vertexAttrib2fv_`

``` purescript
vertexAttrib2fv_ :: forall eff. GLuint -> FloatArray -> Eff (webgl :: WebGl | eff) Unit
```


#### `vertexAttrib3f_`

``` purescript
vertexAttrib3f_ :: forall eff. GLuint -> GLfloat -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit
```


#### `vertexAttrib3fv_`

``` purescript
vertexAttrib3fv_ :: forall eff. GLuint -> FloatArray -> Eff (webgl :: WebGl | eff) Unit
```


#### `vertexAttrib4f_`

``` purescript
vertexAttrib4f_ :: forall eff. GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit
```


#### `vertexAttrib4fv_`

``` purescript
vertexAttrib4fv_ :: forall eff. GLuint -> FloatArray -> Eff (webgl :: WebGl | eff) Unit
```


#### `vertexAttribPointer_`

``` purescript
vertexAttribPointer_ :: forall eff. GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> Eff (webgl :: WebGl | eff) Unit
```


#### `viewport_`

``` purescript
viewport_ :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit
```



## Module Graphics.WebGLTexture


#### `WebGLTex`

``` purescript
newtype WebGLTex
  = WebGLTex WebGLTexture
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


#### `InternalFormat`

``` purescript
data InternalFormat
  = IF_ALPHA 
  | IF_LUMINANCE 
  | IF_LUMINANCE_ALPHA 
  | IF_RGB 
  | IF_RGBA 
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


#### `SymbolicParameter`

``` purescript
data SymbolicParameter
  = PACK_ALIGNMENT 
  | UNPACK_ALIGNMENT 
  | UNPACK_FLIP_Y_WEBGL 
  | UNPACK_PREMULTIPLY_ALPHA_WEBGL 
  | UNPACK_COLORSPACE_CONVERSION_WEBGL 
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
  | TEXTURE_MAX_ANISOTROPY_EXT 
```


#### `TexFilterSpec`

``` purescript
data TexFilterSpec
  = NEAREST 
  | LINEAR 
  | MIPMAP 
```

#### `texture2DFor`

``` purescript
texture2DFor :: forall a eff. String -> TexFilterSpec -> (WebGLTex -> EffWebGL eff a) -> EffWebGL eff Unit
```


#### `handleLoad2D`

``` purescript
handleLoad2D :: forall eff a. WebGLTex -> TexFilterSpec -> a -> EffWebGL eff Unit
```


#### `withTexture2D`

``` purescript
withTexture2D :: forall eff typ. WebGLTex -> Number -> Uniform typ -> Number -> EffWebGL eff Unit
```


#### `bindTexture`

``` purescript
bindTexture :: forall eff. TargetType -> WebGLTex -> EffWebGL eff Unit
```


#### `activeTexture`

``` purescript
activeTexture :: forall eff. Number -> Eff (webgl :: WebGl | eff) Unit
```


#### `createTexture`

``` purescript
createTexture :: forall eff. Eff (webgl :: WebGl | eff) WebGLTex
```