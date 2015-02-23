# Module Documentation

## Module Control.Monad.Eff.WebGL

### Types

#### `EffWebGL`

    type EffWebGL eff a = Eff (webgl :: WebGl | eff) a

#### `WebGl`

    data WebGl :: !


### Values

#### `runWebGl_`

    runWebGl_ :: forall a e. Eff (webgl :: WebGl | e) a -> Eff e a


## Module Graphics.Canvas.Extended

### Types

#### `Image`

    data Image :: *


## Module Graphics.WebGL

### Types

#### `Attribute`

    newtype Attribute typ
      = Attribute { aItemSize :: Number, aItemType :: Number, aName :: String, aLocation :: GLint }

#### `Bool`

    data Bool

#### `Buffer`

    type Buffer a = { bufferSize :: Number, bufferType :: Number, webGLBuffer :: WebGLBuffer }

#### `BufferTarget`

    data BufferTarget
      = ARRAY_BUFFER 
      | ELEMENT_ARRAY_BUFFER 

#### `Capacity`

     * Constants

    data Capacity
      = BLEND 
      | DEPTH_TEST 
      | CULL_FACE 
      | POLYGON_OFFSET_FILL 
      | SCISSOR_TEST 

#### `Mask`

    data Mask
      = DEPTH_BUFFER_BIT 
      | STENCIL_BUFFER_BIT 
      | COLOR_BUFFER_BIT 

#### `Mat2`

    data Mat2

#### `Mat3`

    data Mat3

#### `Mat4`

    data Mat4

#### `Mode`

    data Mode
      = POINTS 
      | LINES 
      | LINE_STRIP 
      | LINE_LOOP 
      | TRIANGLES 
      | TRIANGLE_STRIP 
      | TRIANGLE_FAN 

#### `Sampler2D`

    data Sampler2D

#### `Shaders`

    data Shaders bindings
      = Shaders String String

#### `Uniform`

    newtype Uniform typ
      = Uniform { uType :: Number, uName :: String, uLocation :: WebGLUniformLocation }

#### `Vec2`

    data Vec2

#### `Vec3`

    data Vec3

#### `Vec4`

    data Vec4

#### `WebGLContext`

    type WebGLContext = { canvasName :: String }

#### `WebGLProg`

    newtype WebGLProg


### Values

#### `bindBuf`

    bindBuf :: forall a eff. Buffer a -> Eff (webgl :: WebGl | eff) Unit

#### `bindPointBuf`

    bindPointBuf :: forall a eff typ. Buffer a -> Attribute typ -> Eff (webgl :: WebGl | eff) Unit

#### `clear`

    clear :: forall eff. [Mask] -> Eff (webgl :: WebGl | eff) Unit

#### `drawArr`

    drawArr :: forall a eff typ. Mode -> Buffer a -> Attribute typ -> EffWebGL eff Unit

#### `drawElements`

    drawElements :: forall a eff. Mode -> Number -> EffWebGL eff Unit

#### `enable`

    enable :: forall eff. Capacity -> Eff (webgl :: WebGl | eff) Unit

#### `getCanvasHeight`

    getCanvasHeight :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Number

#### `getCanvasWidth`

    getCanvasWidth :: forall eff. WebGLContext -> Eff (webgl :: WebGl | eff) Number

#### `makeBuffer`

    makeBuffer :: forall a eff. BufferTarget -> ([Number] -> T.ArrayBuffer a) -> [Number] -> Eff (webgl :: WebGl | eff) (Buffer a)

#### `makeBufferSimple`

    makeBufferSimple :: forall eff. [Number] -> Eff (webgl :: WebGl | eff) (Buffer T.Float32)

#### `requestAnimationFrame`

    requestAnimationFrame :: forall a eff. Eff (webgl :: WebGl | eff) a -> Eff (webgl :: WebGl | eff) Unit

#### `runWebGL`

     | Returns either a continuation which takes a String in the error case,
       which happens when WebGL is not present, or a (Right) continuation with the WebGL
       effect.

    runWebGL :: forall a eff. String -> (String -> Eff eff a) -> (WebGLContext -> EffWebGL eff a) -> Eff eff a

#### `setUniformBooleans`

    setUniformBooleans :: forall eff typ. Uniform typ -> [Boolean] -> EffWebGL eff Unit

#### `setUniformFloats`

    setUniformFloats :: forall eff typ. Uniform typ -> [Number] -> EffWebGL eff Unit

#### `vertexPointer`

    vertexPointer :: forall eff typ. Attribute typ -> EffWebGL eff Unit

#### `withShaders`

    withShaders :: forall bindings eff a. Shaders (Object bindings) -> (String -> EffWebGL eff a) -> ({ webGLProgram :: WebGLProg | bindings } -> EffWebGL eff a) -> EffWebGL eff a


## Module Graphics.WebGLRaw

### Types

#### `ArrayBufferView`

    data ArrayBufferView :: *

#### `FloatArray`

    type FloatArray = Float32Array

#### `GLbitfield`

    type GLbitfield = Number

#### `GLboolean`

    type GLboolean = Boolean

#### `GLbyte`

    type GLbyte = Number

#### `GLclampf`

    type GLclampf = Number

#### `GLenum`

    type GLenum = Number

#### `GLfloat`

    type GLfloat = Number

#### `GLint`

    type GLint = Number

#### `GLintptr`

    type GLintptr = Number

#### `GLshort`

    type GLshort = Number

#### `GLsizei`

    type GLsizei = Number

#### `GLsizeiptr`

    type GLsizeiptr = Number

#### `GLubyte`

    type GLubyte = Number

#### `GLuint`

    type GLuint = Number

#### `GLushort`

    type GLushort = Number

#### `HTMLImageElement`

    data HTMLImageElement :: *

#### `HTMLVideoElement`

    data HTMLVideoElement :: *

#### `ImageData`

    data ImageData :: *

#### `WebGLActiveInfo`

    data WebGLActiveInfo :: *

#### `WebGLBuffer`

    data WebGLBuffer :: *

#### `WebGLContextAttributes`

     *TypeDecls

    data WebGLContextAttributes :: *

#### `WebGLFramebuffer`

    data WebGLFramebuffer :: *

#### `WebGLProgram`

    data WebGLProgram :: *

#### `WebGLRenderbuffer`

    data WebGLRenderbuffer :: *

#### `WebGLShader`

    data WebGLShader :: *

#### `WebGLTexture`

    data WebGLTexture :: *

#### `WebGLUniformLocation`

    data WebGLUniformLocation :: *


### Values

#### `_ACTIVE_ATTRIBUTES`

    _ACTIVE_ATTRIBUTES :: Number

#### `_ACTIVE_ATTRIBUTE_MAX_LENGTH`

    _ACTIVE_ATTRIBUTE_MAX_LENGTH :: Number

#### `_ACTIVE_TEXTURE`

    _ACTIVE_TEXTURE :: Number

#### `_ACTIVE_UNIFORMS`

    _ACTIVE_UNIFORMS :: Number

#### `_ACTIVE_UNIFORM_MAX_LENGTH`

    _ACTIVE_UNIFORM_MAX_LENGTH :: Number

#### `_ALIASED_LINE_WIDTH_RANGE`

    _ALIASED_LINE_WIDTH_RANGE :: Number

#### `_ALIASED_POINT_SIZE_RANGE`

    _ALIASED_POINT_SIZE_RANGE :: Number

#### `_ALPHA`

    _ALPHA :: Number

#### `_ALPHA_BITS`

    _ALPHA_BITS :: Number

#### `_ALWAYS`

    _ALWAYS :: Number

#### `_ARRAY_BUFFER`

    _ARRAY_BUFFER :: Number

#### `_ARRAY_BUFFER_BINDING`

    _ARRAY_BUFFER_BINDING :: Number

#### `_ATTACHED_SHADERS`

    _ATTACHED_SHADERS :: Number

#### `_BACK`

    _BACK :: Number

#### `_BLEND`

    _BLEND :: Number

#### `_BLEND_COLOR`

    _BLEND_COLOR :: Number

#### `_BLEND_DST_ALPHA`

    _BLEND_DST_ALPHA :: Number

#### `_BLEND_DST_RGB`

    _BLEND_DST_RGB :: Number

#### `_BLEND_EQUATION`

    _BLEND_EQUATION :: Number

#### `_BLEND_EQUATION_ALPHA`

    _BLEND_EQUATION_ALPHA :: Number

#### `_BLEND_EQUATION_RGB`

    _BLEND_EQUATION_RGB :: Number

#### `_BLEND_SRC_ALPHA`

    _BLEND_SRC_ALPHA :: Number

#### `_BLEND_SRC_RGB`

    _BLEND_SRC_RGB :: Number

#### `_BLUE_BITS`

    _BLUE_BITS :: Number

#### `_BOOL`

    _BOOL :: Number

#### `_BOOL_VEC2`

    _BOOL_VEC2 :: Number

#### `_BOOL_VEC3`

    _BOOL_VEC3 :: Number

#### `_BOOL_VEC4`

    _BOOL_VEC4 :: Number

#### `_BROWSER_DEFAULT_WEBGL`

    _BROWSER_DEFAULT_WEBGL :: Number

#### `_BUFFER_SIZE`

    _BUFFER_SIZE :: Number

#### `_BUFFER_USAGE`

    _BUFFER_USAGE :: Number

#### `_BYTE`

    _BYTE :: Number

#### `_CCW`

    _CCW :: Number

#### `_CLAMP_TO_EDGE`

    _CLAMP_TO_EDGE :: Number

#### `_COLOR_ATTACHMENT0`

    _COLOR_ATTACHMENT0 :: Number

#### `_COLOR_BUFFER_BIT`

    _COLOR_BUFFER_BIT :: Number

#### `_COLOR_CLEAR_VALUE`

    _COLOR_CLEAR_VALUE :: Number

#### `_COLOR_WRITEMASK`

    _COLOR_WRITEMASK :: Number

#### `_COMPILE_STATUS`

    _COMPILE_STATUS :: Number

#### `_COMPRESSED_TEXTURE_FORMATS`

    _COMPRESSED_TEXTURE_FORMATS :: Number

#### `_CONSTANT_ALPHA`

    _CONSTANT_ALPHA :: Number

#### `_CONSTANT_COLOR`

    _CONSTANT_COLOR :: Number

#### `_CONTEXT_LOST_WEBGL`

    _CONTEXT_LOST_WEBGL :: Number

#### `_CULL_FACE`

    _CULL_FACE :: Number

#### `_CULL_FACE_MODE`

    _CULL_FACE_MODE :: Number

#### `_CURRENT_PROGRAM`

    _CURRENT_PROGRAM :: Number

#### `_CURRENT_VERTEX_ATTRIB`

    _CURRENT_VERTEX_ATTRIB :: Number

#### `_CW`

    _CW :: Number

#### `_DECR`

    _DECR :: Number

#### `_DECR_WRAP`

    _DECR_WRAP :: Number

#### `_DELETE_STATUS`

    _DELETE_STATUS :: Number

#### `_DEPTH_ATTACHMENT`

    _DEPTH_ATTACHMENT :: Number

#### `_DEPTH_BITS`

    _DEPTH_BITS :: Number

#### `_DEPTH_BUFFER_BIT`

     *Constants

    _DEPTH_BUFFER_BIT :: Number

#### `_DEPTH_CLEAR_VALUE`

    _DEPTH_CLEAR_VALUE :: Number

#### `_DEPTH_COMPONENT`

    _DEPTH_COMPONENT :: Number

#### `_DEPTH_COMPONENT16`

    _DEPTH_COMPONENT16 :: Number

#### `_DEPTH_FUNC`

    _DEPTH_FUNC :: Number

#### `_DEPTH_RANGE`

    _DEPTH_RANGE :: Number

#### `_DEPTH_STENCIL`

    _DEPTH_STENCIL :: Number

#### `_DEPTH_STENCIL_ATTACHMENT`

    _DEPTH_STENCIL_ATTACHMENT :: Number

#### `_DEPTH_TEST`

    _DEPTH_TEST :: Number

#### `_DEPTH_WRITEMASK`

    _DEPTH_WRITEMASK :: Number

#### `_DITHER`

    _DITHER :: Number

#### `_DONT_CARE`

    _DONT_CARE :: Number

#### `_DST_ALPHA`

    _DST_ALPHA :: Number

#### `_DST_COLOR`

    _DST_COLOR :: Number

#### `_DYNAMIC_DRAW`

    _DYNAMIC_DRAW :: Number

#### `_ELEMENT_ARRAY_BUFFER`

    _ELEMENT_ARRAY_BUFFER :: Number

#### `_ELEMENT_ARRAY_BUFFER_BINDING`

    _ELEMENT_ARRAY_BUFFER_BINDING :: Number

#### `_EQUAL`

    _EQUAL :: Number

#### `_FASTEST`

    _FASTEST :: Number

#### `_FLOAT`

    _FLOAT :: Number

#### `_FLOAT_MAT2`

    _FLOAT_MAT2 :: Number

#### `_FLOAT_MAT3`

    _FLOAT_MAT3 :: Number

#### `_FLOAT_MAT4`

    _FLOAT_MAT4 :: Number

#### `_FLOAT_VEC2`

    _FLOAT_VEC2 :: Number

#### `_FLOAT_VEC3`

    _FLOAT_VEC3 :: Number

#### `_FLOAT_VEC4`

    _FLOAT_VEC4 :: Number

#### `_FRAGMENT_SHADER`

    _FRAGMENT_SHADER :: Number

#### `_FRAMEBUFFER`

    _FRAMEBUFFER :: Number

#### `_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME`

    _FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: Number

#### `_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE`

    _FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: Number

#### `_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE`

    _FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: Number

#### `_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL`

    _FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: Number

#### `_FRAMEBUFFER_BINDING`

    _FRAMEBUFFER_BINDING :: Number

#### `_FRAMEBUFFER_COMPLETE`

    _FRAMEBUFFER_COMPLETE :: Number

#### `_FRAMEBUFFER_INCOMPLETE_ATTACHMENT`

    _FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: Number

#### `_FRAMEBUFFER_INCOMPLETE_DIMENSIONS`

    _FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: Number

#### `_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT`

    _FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: Number

#### `_FRAMEBUFFER_UNSUPPORTED`

    _FRAMEBUFFER_UNSUPPORTED :: Number

#### `_FRONT`

    _FRONT :: Number

#### `_FRONT_AND_BACK`

    _FRONT_AND_BACK :: Number

#### `_FRONT_FACE`

    _FRONT_FACE :: Number

#### `_FUNC_ADD`

    _FUNC_ADD :: Number

#### `_FUNC_REVERSE_SUBTRACT`

    _FUNC_REVERSE_SUBTRACT :: Number

#### `_FUNC_SUBTRACT`

    _FUNC_SUBTRACT :: Number

#### `_GENERATE_MIPMAP_HINT`

    _GENERATE_MIPMAP_HINT :: Number

#### `_GEQUAL`

    _GEQUAL :: Number

#### `_GREATER`

    _GREATER :: Number

#### `_GREEN_BITS`

    _GREEN_BITS :: Number

#### `_HIGH_FLOAT`

    _HIGH_FLOAT :: Number

#### `_HIGH_INT`

    _HIGH_INT :: Number

#### `_INCR`

    _INCR :: Number

#### `_INCR_WRAP`

    _INCR_WRAP :: Number

#### `_INFO_LOG_LENGTH`

    _INFO_LOG_LENGTH :: Number

#### `_INT`

    _INT :: Number

#### `_INT_VEC2`

    _INT_VEC2 :: Number

#### `_INT_VEC3`

    _INT_VEC3 :: Number

#### `_INT_VEC4`

    _INT_VEC4 :: Number

#### `_INVALID_ENUM`

    _INVALID_ENUM :: Number

#### `_INVALID_FRAMEBUFFER_OPERATION`

    _INVALID_FRAMEBUFFER_OPERATION :: Number

#### `_INVALID_OPERATION`

    _INVALID_OPERATION :: Number

#### `_INVALID_VALUE`

    _INVALID_VALUE :: Number

#### `_INVERT`

    _INVERT :: Number

#### `_KEEP`

    _KEEP :: Number

#### `_LEQUAL`

    _LEQUAL :: Number

#### `_LESS`

    _LESS :: Number

#### `_LINEAR`

    _LINEAR :: Number

#### `_LINEAR_MIPMAP_LINEAR`

    _LINEAR_MIPMAP_LINEAR :: Number

#### `_LINEAR_MIPMAP_NEAREST`

    _LINEAR_MIPMAP_NEAREST :: Number

#### `_LINES`

    _LINES :: Number

#### `_LINE_LOOP`

    _LINE_LOOP :: Number

#### `_LINE_STRIP`

    _LINE_STRIP :: Number

#### `_LINE_WIDTH`

    _LINE_WIDTH :: Number

#### `_LINK_STATUS`

    _LINK_STATUS :: Number

#### `_LOW_FLOAT`

    _LOW_FLOAT :: Number

#### `_LOW_INT`

    _LOW_INT :: Number

#### `_LUMINANCE`

    _LUMINANCE :: Number

#### `_LUMINANCE_ALPHA`

    _LUMINANCE_ALPHA :: Number

#### `_MAX_COMBINED_TEXTURE_IMAGE_UNITS`

    _MAX_COMBINED_TEXTURE_IMAGE_UNITS :: Number

#### `_MAX_CUBE_MAP_TEXTURE_SIZE`

    _MAX_CUBE_MAP_TEXTURE_SIZE :: Number

#### `_MAX_FRAGMENT_UNIFORM_VECTORS`

    _MAX_FRAGMENT_UNIFORM_VECTORS :: Number

#### `_MAX_RENDERBUFFER_SIZE`

    _MAX_RENDERBUFFER_SIZE :: Number

#### `_MAX_TEXTURE_IMAGE_UNITS`

    _MAX_TEXTURE_IMAGE_UNITS :: Number

#### `_MAX_TEXTURE_SIZE`

    _MAX_TEXTURE_SIZE :: Number

#### `_MAX_VARYING_VECTORS`

    _MAX_VARYING_VECTORS :: Number

#### `_MAX_VERTEX_ATTRIBS`

    _MAX_VERTEX_ATTRIBS :: Number

#### `_MAX_VERTEX_TEXTURE_IMAGE_UNITS`

    _MAX_VERTEX_TEXTURE_IMAGE_UNITS :: Number

#### `_MAX_VERTEX_UNIFORM_VECTORS`

    _MAX_VERTEX_UNIFORM_VECTORS :: Number

#### `_MAX_VIEWPORT_DIMS`

    _MAX_VIEWPORT_DIMS :: Number

#### `_MEDIUM_FLOAT`

    _MEDIUM_FLOAT :: Number

#### `_MEDIUM_INT`

    _MEDIUM_INT :: Number

#### `_MIRRORED_REPEAT`

    _MIRRORED_REPEAT :: Number

#### `_NEAREST`

    _NEAREST :: Number

#### `_NEAREST_MIPMAP_LINEAR`

    _NEAREST_MIPMAP_LINEAR :: Number

#### `_NEAREST_MIPMAP_NEAREST`

    _NEAREST_MIPMAP_NEAREST :: Number

#### `_NEVER`

    _NEVER :: Number

#### `_NICEST`

    _NICEST :: Number

#### `_NONE`

    _NONE :: Number

#### `_NOTEQUAL`

    _NOTEQUAL :: Number

#### `_NO_ERROR`

    _NO_ERROR :: Number

#### `_NUM_COMPRESSED_TEXTURE_FORMATS`

    _NUM_COMPRESSED_TEXTURE_FORMATS :: Number

#### `_ONE`

    _ONE :: Number

#### `_ONE_MINUS_CONSTANT_ALPHA`

    _ONE_MINUS_CONSTANT_ALPHA :: Number

#### `_ONE_MINUS_CONSTANT_COLOR`

    _ONE_MINUS_CONSTANT_COLOR :: Number

#### `_ONE_MINUS_DST_ALPHA`

    _ONE_MINUS_DST_ALPHA :: Number

#### `_ONE_MINUS_DST_COLOR`

    _ONE_MINUS_DST_COLOR :: Number

#### `_ONE_MINUS_SRC_ALPHA`

    _ONE_MINUS_SRC_ALPHA :: Number

#### `_ONE_MINUS_SRC_COLOR`

    _ONE_MINUS_SRC_COLOR :: Number

#### `_OUT_OF_MEMORY`

    _OUT_OF_MEMORY :: Number

#### `_PACK_ALIGNMENT`

    _PACK_ALIGNMENT :: Number

#### `_POINTS`

    _POINTS :: Number

#### `_POLYGON_OFFSET_FACTOR`

    _POLYGON_OFFSET_FACTOR :: Number

#### `_POLYGON_OFFSET_FILL`

    _POLYGON_OFFSET_FILL :: Number

#### `_POLYGON_OFFSET_UNITS`

    _POLYGON_OFFSET_UNITS :: Number

#### `_RED_BITS`

    _RED_BITS :: Number

#### `_RENDERBUFFER`

    _RENDERBUFFER :: Number

#### `_RENDERBUFFER_ALPHA_SIZE`

    _RENDERBUFFER_ALPHA_SIZE :: Number

#### `_RENDERBUFFER_BINDING`

    _RENDERBUFFER_BINDING :: Number

#### `_RENDERBUFFER_BLUE_SIZE`

    _RENDERBUFFER_BLUE_SIZE :: Number

#### `_RENDERBUFFER_DEPTH_SIZE`

    _RENDERBUFFER_DEPTH_SIZE :: Number

#### `_RENDERBUFFER_GREEN_SIZE`

    _RENDERBUFFER_GREEN_SIZE :: Number

#### `_RENDERBUFFER_HEIGHT`

    _RENDERBUFFER_HEIGHT :: Number

#### `_RENDERBUFFER_INTERNAL_FORMAT`

    _RENDERBUFFER_INTERNAL_FORMAT :: Number

#### `_RENDERBUFFER_RED_SIZE`

    _RENDERBUFFER_RED_SIZE :: Number

#### `_RENDERBUFFER_STENCIL_SIZE`

    _RENDERBUFFER_STENCIL_SIZE :: Number

#### `_RENDERBUFFER_WIDTH`

    _RENDERBUFFER_WIDTH :: Number

#### `_RENDERER`

    _RENDERER :: Number

#### `_REPEAT`

    _REPEAT :: Number

#### `_REPLACE`

    _REPLACE :: Number

#### `_RGB`

    _RGB :: Number

#### `_RGB565`

    _RGB565 :: Number

#### `_RGB5_A1`

    _RGB5_A1 :: Number

#### `_RGBA`

    _RGBA :: Number

#### `_RGBA4`

    _RGBA4 :: Number

#### `_SAMPLER_2D`

    _SAMPLER_2D :: Number

#### `_SAMPLER_CUBE`

    _SAMPLER_CUBE :: Number

#### `_SAMPLES`

    _SAMPLES :: Number

#### `_SAMPLE_ALPHA_TO_COVERAGE`

    _SAMPLE_ALPHA_TO_COVERAGE :: Number

#### `_SAMPLE_BUFFERS`

    _SAMPLE_BUFFERS :: Number

#### `_SAMPLE_COVERAGE`

    _SAMPLE_COVERAGE :: Number

#### `_SAMPLE_COVERAGE_INVERT`

    _SAMPLE_COVERAGE_INVERT :: Number

#### `_SAMPLE_COVERAGE_VALUE`

    _SAMPLE_COVERAGE_VALUE :: Number

#### `_SCISSOR_BOX`

    _SCISSOR_BOX :: Number

#### `_SCISSOR_TEST`

    _SCISSOR_TEST :: Number

#### `_SHADER_SOURCE_LENGTH`

    _SHADER_SOURCE_LENGTH :: Number

#### `_SHADER_TYPE`

    _SHADER_TYPE :: Number

#### `_SHADING_LANGUAGE_VERSION`

    _SHADING_LANGUAGE_VERSION :: Number

#### `_SHORT`

    _SHORT :: Number

#### `_SRC_ALPHA`

    _SRC_ALPHA :: Number

#### `_SRC_ALPHA_SATURATE`

    _SRC_ALPHA_SATURATE :: Number

#### `_SRC_COLOR`

    _SRC_COLOR :: Number

#### `_STATIC_DRAW`

    _STATIC_DRAW :: Number

#### `_STENCIL_ATTACHMENT`

    _STENCIL_ATTACHMENT :: Number

#### `_STENCIL_BACK_FAIL`

    _STENCIL_BACK_FAIL :: Number

#### `_STENCIL_BACK_FUNC`

    _STENCIL_BACK_FUNC :: Number

#### `_STENCIL_BACK_PASS_DEPTH_FAIL`

    _STENCIL_BACK_PASS_DEPTH_FAIL :: Number

#### `_STENCIL_BACK_PASS_DEPTH_PASS`

    _STENCIL_BACK_PASS_DEPTH_PASS :: Number

#### `_STENCIL_BACK_REF`

    _STENCIL_BACK_REF :: Number

#### `_STENCIL_BACK_VALUE_MASK`

    _STENCIL_BACK_VALUE_MASK :: Number

#### `_STENCIL_BACK_WRITEMASK`

    _STENCIL_BACK_WRITEMASK :: Number

#### `_STENCIL_BITS`

    _STENCIL_BITS :: Number

#### `_STENCIL_BUFFER_BIT`

    _STENCIL_BUFFER_BIT :: Number

#### `_STENCIL_CLEAR_VALUE`

    _STENCIL_CLEAR_VALUE :: Number

#### `_STENCIL_FAIL`

    _STENCIL_FAIL :: Number

#### `_STENCIL_FUNC`

    _STENCIL_FUNC :: Number

#### `_STENCIL_INDEX`

    _STENCIL_INDEX :: Number

#### `_STENCIL_INDEX8`

    _STENCIL_INDEX8 :: Number

#### `_STENCIL_PASS_DEPTH_FAIL`

    _STENCIL_PASS_DEPTH_FAIL :: Number

#### `_STENCIL_PASS_DEPTH_PASS`

    _STENCIL_PASS_DEPTH_PASS :: Number

#### `_STENCIL_REF`

    _STENCIL_REF :: Number

#### `_STENCIL_TEST`

    _STENCIL_TEST :: Number

#### `_STENCIL_VALUE_MASK`

    _STENCIL_VALUE_MASK :: Number

#### `_STENCIL_WRITEMASK`

    _STENCIL_WRITEMASK :: Number

#### `_STREAM_DRAW`

    _STREAM_DRAW :: Number

#### `_SUBPIXEL_BITS`

    _SUBPIXEL_BITS :: Number

#### `_TEXTURE`

    _TEXTURE :: Number

#### `_TEXTURE0`

    _TEXTURE0 :: Number

#### `_TEXTURE1`

    _TEXTURE1 :: Number

#### `_TEXTURE10`

    _TEXTURE10 :: Number

#### `_TEXTURE11`

    _TEXTURE11 :: Number

#### `_TEXTURE12`

    _TEXTURE12 :: Number

#### `_TEXTURE13`

    _TEXTURE13 :: Number

#### `_TEXTURE14`

    _TEXTURE14 :: Number

#### `_TEXTURE15`

    _TEXTURE15 :: Number

#### `_TEXTURE16`

    _TEXTURE16 :: Number

#### `_TEXTURE17`

    _TEXTURE17 :: Number

#### `_TEXTURE18`

    _TEXTURE18 :: Number

#### `_TEXTURE19`

    _TEXTURE19 :: Number

#### `_TEXTURE2`

    _TEXTURE2 :: Number

#### `_TEXTURE20`

    _TEXTURE20 :: Number

#### `_TEXTURE21`

    _TEXTURE21 :: Number

#### `_TEXTURE22`

    _TEXTURE22 :: Number

#### `_TEXTURE23`

    _TEXTURE23 :: Number

#### `_TEXTURE24`

    _TEXTURE24 :: Number

#### `_TEXTURE25`

    _TEXTURE25 :: Number

#### `_TEXTURE26`

    _TEXTURE26 :: Number

#### `_TEXTURE27`

    _TEXTURE27 :: Number

#### `_TEXTURE28`

    _TEXTURE28 :: Number

#### `_TEXTURE29`

    _TEXTURE29 :: Number

#### `_TEXTURE3`

    _TEXTURE3 :: Number

#### `_TEXTURE30`

    _TEXTURE30 :: Number

#### `_TEXTURE31`

    _TEXTURE31 :: Number

#### `_TEXTURE4`

    _TEXTURE4 :: Number

#### `_TEXTURE5`

    _TEXTURE5 :: Number

#### `_TEXTURE6`

    _TEXTURE6 :: Number

#### `_TEXTURE7`

    _TEXTURE7 :: Number

#### `_TEXTURE8`

    _TEXTURE8 :: Number

#### `_TEXTURE9`

    _TEXTURE9 :: Number

#### `_TEXTURE_2D`

    _TEXTURE_2D :: Number

#### `_TEXTURE_BINDING_2D`

    _TEXTURE_BINDING_2D :: Number

#### `_TEXTURE_BINDING_CUBE_MAP`

    _TEXTURE_BINDING_CUBE_MAP :: Number

#### `_TEXTURE_CUBE_MAP`

    _TEXTURE_CUBE_MAP :: Number

#### `_TEXTURE_CUBE_MAP_NEGATIVE_X`

    _TEXTURE_CUBE_MAP_NEGATIVE_X :: Number

#### `_TEXTURE_CUBE_MAP_NEGATIVE_Y`

    _TEXTURE_CUBE_MAP_NEGATIVE_Y :: Number

#### `_TEXTURE_CUBE_MAP_NEGATIVE_Z`

    _TEXTURE_CUBE_MAP_NEGATIVE_Z :: Number

#### `_TEXTURE_CUBE_MAP_POSITIVE_X`

    _TEXTURE_CUBE_MAP_POSITIVE_X :: Number

#### `_TEXTURE_CUBE_MAP_POSITIVE_Y`

    _TEXTURE_CUBE_MAP_POSITIVE_Y :: Number

#### `_TEXTURE_CUBE_MAP_POSITIVE_Z`

    _TEXTURE_CUBE_MAP_POSITIVE_Z :: Number

#### `_TEXTURE_MAG_FILTER`

    _TEXTURE_MAG_FILTER :: Number

#### `_TEXTURE_MIN_FILTER`

    _TEXTURE_MIN_FILTER :: Number

#### `_TEXTURE_WRAP_S`

    _TEXTURE_WRAP_S :: Number

#### `_TEXTURE_WRAP_T`

    _TEXTURE_WRAP_T :: Number

#### `_TRIANGLES`

    _TRIANGLES :: Number

#### `_TRIANGLE_FAN`

    _TRIANGLE_FAN :: Number

#### `_TRIANGLE_STRIP`

    _TRIANGLE_STRIP :: Number

#### `_UNPACK_ALIGNMENT`

    _UNPACK_ALIGNMENT :: Number

#### `_UNPACK_COLORSPACE_CONVERSION_WEBGL`

    _UNPACK_COLORSPACE_CONVERSION_WEBGL :: Number

#### `_UNPACK_FLIP_Y_WEBGL`

    _UNPACK_FLIP_Y_WEBGL :: Number

#### `_UNPACK_PREMULTIPLY_ALPHA_WEBGL`

    _UNPACK_PREMULTIPLY_ALPHA_WEBGL :: Number

#### `_UNSIGNED_BYTE`

    _UNSIGNED_BYTE :: Number

#### `_UNSIGNED_INT`

    _UNSIGNED_INT :: Number

#### `_UNSIGNED_SHORT`

    _UNSIGNED_SHORT :: Number

#### `_UNSIGNED_SHORT_4_4_4_4`

    _UNSIGNED_SHORT_4_4_4_4 :: Number

#### `_UNSIGNED_SHORT_5_5_5_1`

    _UNSIGNED_SHORT_5_5_5_1 :: Number

#### `_UNSIGNED_SHORT_5_6_5`

    _UNSIGNED_SHORT_5_6_5 :: Number

#### `_VALIDATE_STATUS`

    _VALIDATE_STATUS :: Number

#### `_VENDOR`

    _VENDOR :: Number

#### `_VERSION`

    _VERSION :: Number

#### `_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING`

    _VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: Number

#### `_VERTEX_ATTRIB_ARRAY_ENABLED`

    _VERTEX_ATTRIB_ARRAY_ENABLED :: Number

#### `_VERTEX_ATTRIB_ARRAY_NORMALIZED`

    _VERTEX_ATTRIB_ARRAY_NORMALIZED :: Number

#### `_VERTEX_ATTRIB_ARRAY_POINTER`

    _VERTEX_ATTRIB_ARRAY_POINTER :: Number

#### `_VERTEX_ATTRIB_ARRAY_SIZE`

    _VERTEX_ATTRIB_ARRAY_SIZE :: Number

#### `_VERTEX_ATTRIB_ARRAY_STRIDE`

    _VERTEX_ATTRIB_ARRAY_STRIDE :: Number

#### `_VERTEX_ATTRIB_ARRAY_TYPE`

    _VERTEX_ATTRIB_ARRAY_TYPE :: Number

#### `_VERTEX_SHADER`

    _VERTEX_SHADER :: Number

#### `_VIEWPORT`

    _VIEWPORT :: Number

#### `_ZERO`

    _ZERO :: Number

#### `activeTexture_`

    activeTexture_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `attachShader_`

    attachShader_ :: forall eff. WebGLProgram -> WebGLShader -> Eff (webgl :: WebGl | eff) Unit

#### `bindAttribLocation_`

    bindAttribLocation_ :: forall eff. WebGLProgram -> GLuint -> String -> Eff (webgl :: WebGl | eff) Unit

#### `bindBuffer_`

    bindBuffer_ :: forall eff. GLenum -> WebGLBuffer -> Eff (webgl :: WebGl | eff) Unit

#### `bindFramebuffer_`

    bindFramebuffer_ :: forall eff. GLenum -> WebGLFramebuffer -> Eff (webgl :: WebGl | eff) Unit

#### `bindRenderbuffer_`

    bindRenderbuffer_ :: forall eff. GLenum -> WebGLRenderbuffer -> Eff (webgl :: WebGl | eff) Unit

#### `bindTexture_`

    bindTexture_ :: forall eff. GLenum -> WebGLTexture -> Eff (webgl :: WebGl | eff) Unit

#### `blendColor_`

    blendColor_ :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit

#### `blendEquationSeparate_`

    blendEquationSeparate_ :: forall eff. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `blendEquation_`

    blendEquation_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `blendFuncSeparate_`

    blendFuncSeparate_ :: forall eff. GLenum -> GLenum -> GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `blendFunc_`

    blendFunc_ :: forall eff. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `bufferData_`

    bufferData_ :: forall eff. GLenum -> ArrayBuffer Float32 -> GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `bufferSubData_`

    bufferSubData_ :: forall eff. GLenum -> GLintptr -> ArrayBuffer Float32 -> Eff (webgl :: WebGl | eff) Unit

#### `checkFramebufferStatus_`

    checkFramebufferStatus_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) GLenum

#### `clearColor_`

    clearColor_ :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit

#### `clearDepth_`

    clearDepth_ :: forall eff. GLclampf -> Eff (webgl :: WebGl | eff) Unit

#### `clearStencil_`

    clearStencil_ :: forall eff. GLint -> Eff (webgl :: WebGl | eff) Unit

#### `clear_`

    clear_ :: forall eff. GLbitfield -> Eff (webgl :: WebGl | eff) Unit

#### `colorMask_`

    colorMask_ :: forall eff. GLboolean -> GLboolean -> GLboolean -> GLboolean -> Eff (webgl :: WebGl | eff) Unit

#### `compileShader_`

    compileShader_ :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) Unit

#### `copyTexImage2D_`

    copyTexImage2D_ :: forall eff. GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> Eff (webgl :: WebGl | eff) Unit

#### `copyTexSubImage2D_`

    copyTexSubImage2D_ :: forall eff. GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit

#### `createBuffer_`

    createBuffer_ :: forall eff. Eff (webgl :: WebGl | eff) WebGLBuffer

#### `createFramebuffer_`

    createFramebuffer_ :: forall eff. Eff (webgl :: WebGl | eff) WebGLFramebuffer

#### `createProgram_`

    createProgram_ :: forall eff. Eff (webgl :: WebGl | eff) WebGLProgram

#### `createRenderbuffer_`

    createRenderbuffer_ :: forall eff. Eff (webgl :: WebGl | eff) WebGLRenderbuffer

#### `createShader_`

    createShader_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) WebGLShader

#### `createTexture_`

    createTexture_ :: forall eff. Eff (webgl :: WebGl | eff) WebGLTexture

#### `cullFace_`

    cullFace_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `deleteBuffer_`

    deleteBuffer_ :: forall eff. WebGLBuffer -> Eff (webgl :: WebGl | eff) Unit

#### `deleteFramebuffer_`

    deleteFramebuffer_ :: forall eff. WebGLFramebuffer -> Eff (webgl :: WebGl | eff) Unit

#### `deleteProgram_`

    deleteProgram_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) Unit

#### `deleteRenderbuffer_`

    deleteRenderbuffer_ :: forall eff. WebGLRenderbuffer -> Eff (webgl :: WebGl | eff) Unit

#### `deleteShader_`

    deleteShader_ :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) Unit

#### `deleteTexture_`

    deleteTexture_ :: forall eff. WebGLTexture -> Eff (webgl :: WebGl | eff) Unit

#### `depthFunc_`

    depthFunc_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `depthMask_`

    depthMask_ :: forall eff. GLboolean -> Eff (webgl :: WebGl | eff) Unit

#### `depthRange_`

    depthRange_ :: forall eff. GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit

#### `detachShader_`

    detachShader_ :: forall eff. WebGLProgram -> WebGLShader -> Eff (webgl :: WebGl | eff) Unit

#### `disableVertexAttribArray_`

    disableVertexAttribArray_ :: forall eff. GLuint -> Eff (webgl :: WebGl | eff) Unit

#### `disable_`

    disable_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `drawArrays_`

    drawArrays_ :: forall eff. GLenum -> GLint -> GLsizei -> Eff (webgl :: WebGl | eff) Unit

#### `drawElements_`

    drawElements_ :: forall eff. GLenum -> GLsizei -> GLenum -> GLintptr -> Eff (webgl :: WebGl | eff) Unit

#### `enableVertexAttribArray_`

    enableVertexAttribArray_ :: forall eff. GLuint -> Eff (webgl :: WebGl | eff) Unit

#### `enable_`

    enable_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `finish_`

    finish_ :: forall eff. Eff (webgl :: WebGl | eff) Unit

#### `flush_`

    flush_ :: forall eff. Eff (webgl :: WebGl | eff) Unit

#### `framebufferRenderbuffer_`

    framebufferRenderbuffer_ :: forall eff. GLenum -> GLenum -> GLenum -> WebGLRenderbuffer -> Eff (webgl :: WebGl | eff) Unit

#### `framebufferTexture2D_`

    framebufferTexture2D_ :: forall eff. GLenum -> GLenum -> GLenum -> WebGLTexture -> GLint -> Eff (webgl :: WebGl | eff) Unit

#### `frontFace_`

    frontFace_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `generateMipmap_`

    generateMipmap_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `getActiveAttrib_`

    getActiveAttrib_ :: forall eff. WebGLProgram -> GLuint -> Eff (webgl :: WebGl | eff) WebGLActiveInfo

#### `getActiveUniform_`

    getActiveUniform_ :: forall eff. WebGLProgram -> GLuint -> Eff (webgl :: WebGl | eff) WebGLActiveInfo

#### `getAttachedShaders_`

    getAttachedShaders_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) WebGLShader

#### `getAttribLocation_`

    getAttribLocation_ :: forall eff. WebGLProgram -> String -> Eff (webgl :: WebGl | eff) GLint

#### `getBufferParameter_`

    getBufferParameter_ :: forall eff ret. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) ret

#### `getContextAttributes_`

     *Methods

    getContextAttributes_ :: forall eff. Eff (webgl :: WebGl | eff) WebGLContextAttributes

#### `getError_`

    getError_ :: forall eff. Eff (webgl :: WebGl | eff) GLenum

#### `getExtension_`

    getExtension_ :: forall eff ret. String -> Eff (webgl :: WebGl | eff) ret

#### `getFramebufferAttachmentParameter_`

    getFramebufferAttachmentParameter_ :: forall eff ret. GLenum -> GLenum -> GLenum -> Eff (webgl :: WebGl | eff) ret

#### `getParameter_`

    getParameter_ :: forall eff ret. GLenum -> Eff (webgl :: WebGl | eff) ret

#### `getProgramInfoLog_`

    getProgramInfoLog_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) String

#### `getProgramParameter_`

    getProgramParameter_ :: forall eff ret. WebGLProgram -> GLenum -> Eff (webgl :: WebGl | eff) ret

#### `getRenderbufferParameter_`

    getRenderbufferParameter_ :: forall eff ret. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) ret

#### `getShaderInfoLog_`

    getShaderInfoLog_ :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) String

#### `getShaderParameter_`

    getShaderParameter_ :: forall eff ret. WebGLShader -> GLenum -> Eff (webgl :: WebGl | eff) ret

#### `getShaderSource_`

    getShaderSource_ :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) String

#### `getSupportedExtensions_`

    getSupportedExtensions_ :: forall eff. Eff (webgl :: WebGl | eff) String

#### `getTexParameter_`

    getTexParameter_ :: forall eff ret. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) ret

#### `getUniformLocation_`

    getUniformLocation_ :: forall eff. WebGLProgram -> String -> Eff (webgl :: WebGl | eff) WebGLUniformLocation

#### `getUniform_`

    getUniform_ :: forall eff ret. WebGLProgram -> WebGLUniformLocation -> Eff (webgl :: WebGl | eff) ret

#### `getVertexAttribOffset_`

    getVertexAttribOffset_ :: forall eff. GLuint -> GLenum -> Eff (webgl :: WebGl | eff) GLsizeiptr

#### `getVertexAttrib_`

    getVertexAttrib_ :: forall eff ret. GLuint -> GLenum -> Eff (webgl :: WebGl | eff) ret

#### `hint_`

    hint_ :: forall eff. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `isBuffer_`

    isBuffer_ :: forall eff. WebGLBuffer -> Eff (webgl :: WebGl | eff) GLboolean

#### `isContextLost_`

    isContextLost_ :: forall eff. Eff (webgl :: WebGl | eff) Boolean

#### `isEnabled_`

    isEnabled_ :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) GLboolean

#### `isFramebuffer_`

    isFramebuffer_ :: forall eff. WebGLFramebuffer -> Eff (webgl :: WebGl | eff) GLboolean

#### `isProgram_`

    isProgram_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) GLboolean

#### `isRenderbuffer_`

    isRenderbuffer_ :: forall eff. WebGLRenderbuffer -> Eff (webgl :: WebGl | eff) GLboolean

#### `isShader_`

    isShader_ :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) GLboolean

#### `isTexture_`

    isTexture_ :: forall eff. WebGLTexture -> Eff (webgl :: WebGl | eff) GLboolean

#### `lineWidth_`

    lineWidth_ :: forall eff. GLfloat -> Eff (webgl :: WebGl | eff) Unit

#### `linkProgram_`

    linkProgram_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) Unit

#### `pixelStorei_`

    pixelStorei_ :: forall eff. GLenum -> GLint -> Eff (webgl :: WebGl | eff) Unit

#### `polygonOffset_`

    polygonOffset_ :: forall eff. GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

#### `readPixels_`

    readPixels_ :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> ArrayBufferView -> Eff (webgl :: WebGl | eff) Unit

#### `renderbufferStorage_`

    renderbufferStorage_ :: forall eff. GLenum -> GLenum -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit

#### `sampleCoverage_`

    sampleCoverage_ :: forall eff. GLclampf -> GLboolean -> Eff (webgl :: WebGl | eff) Unit

#### `scissor_`

    scissor_ :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit

#### `shaderSource_`

    shaderSource_ :: forall eff. WebGLShader -> String -> Eff (webgl :: WebGl | eff) Unit

#### `stencilFuncSeparate_`

    stencilFuncSeparate_ :: forall eff. GLenum -> GLenum -> GLint -> GLuint -> Eff (webgl :: WebGl | eff) Unit

#### `stencilFunc_`

    stencilFunc_ :: forall eff. GLenum -> GLint -> GLuint -> Eff (webgl :: WebGl | eff) Unit

#### `stencilMaskSeparate_`

    stencilMaskSeparate_ :: forall eff. GLenum -> GLuint -> Eff (webgl :: WebGl | eff) Unit

#### `stencilMask_`

    stencilMask_ :: forall eff. GLuint -> Eff (webgl :: WebGl | eff) Unit

#### `stencilOpSeparate_`

    stencilOpSeparate_ :: forall eff. GLenum -> GLenum -> GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `stencilOp_`

    stencilOp_ :: forall eff. GLenum -> GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit

#### `texImage2D_`

    texImage2D_ :: forall eff. GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> ArrayBufferView -> Eff (webgl :: WebGl | eff) Unit

#### `texParameterf_`

    texParameterf_ :: forall eff. GLenum -> GLenum -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

#### `texParameteri_`

    texParameteri_ :: forall eff. GLenum -> GLenum -> GLint -> Eff (webgl :: WebGl | eff) Unit

#### `texSubImage2D_`

    texSubImage2D_ :: forall eff. GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> ArrayBufferView -> Eff (webgl :: WebGl | eff) Unit

#### `uniform1f_`

    uniform1f_ :: forall eff. WebGLUniformLocation -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

#### `uniform1fv_`

    uniform1fv_ :: forall eff. WebGLUniformLocation -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

#### `uniform1i_`

    uniform1i_ :: forall eff. WebGLUniformLocation -> GLint -> Eff (webgl :: WebGl | eff) Unit

#### `uniform1iv_`

    uniform1iv_ :: forall eff. WebGLUniformLocation -> Int32Array -> Eff (webgl :: WebGl | eff) Unit

#### `uniform2f_`

    uniform2f_ :: forall eff. WebGLUniformLocation -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

#### `uniform2fv_`

    uniform2fv_ :: forall eff. WebGLUniformLocation -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

#### `uniform2i_`

    uniform2i_ :: forall eff. WebGLUniformLocation -> GLint -> GLint -> Eff (webgl :: WebGl | eff) Unit

#### `uniform2iv_`

    uniform2iv_ :: forall eff. WebGLUniformLocation -> Int32Array -> Eff (webgl :: WebGl | eff) Unit

#### `uniform3f_`

    uniform3f_ :: forall eff. WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

#### `uniform3fv_`

    uniform3fv_ :: forall eff. WebGLUniformLocation -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

#### `uniform3i_`

    uniform3i_ :: forall eff. WebGLUniformLocation -> GLint -> GLint -> GLint -> Eff (webgl :: WebGl | eff) Unit

#### `uniform3iv_`

    uniform3iv_ :: forall eff. WebGLUniformLocation -> Int32Array -> Eff (webgl :: WebGl | eff) Unit

#### `uniform4f_`

    uniform4f_ :: forall eff. WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

#### `uniform4fv_`

    uniform4fv_ :: forall eff. WebGLUniformLocation -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

#### `uniform4i_`

    uniform4i_ :: forall eff. WebGLUniformLocation -> GLint -> GLint -> GLint -> GLint -> Eff (webgl :: WebGl | eff) Unit

#### `uniform4iv_`

    uniform4iv_ :: forall eff. WebGLUniformLocation -> Int32Array -> Eff (webgl :: WebGl | eff) Unit

#### `uniformMatrix2fv_`

    uniformMatrix2fv_ :: forall eff. WebGLUniformLocation -> GLboolean -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

#### `uniformMatrix3fv_`

    uniformMatrix3fv_ :: forall eff. WebGLUniformLocation -> GLboolean -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

#### `uniformMatrix4fv_`

    uniformMatrix4fv_ :: forall eff. WebGLUniformLocation -> GLboolean -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

#### `useProgram_`

    useProgram_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) Unit

#### `validateProgram_`

    validateProgram_ :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) Unit

#### `vertexAttrib1f_`

    vertexAttrib1f_ :: forall eff. GLuint -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

#### `vertexAttrib1fv_`

    vertexAttrib1fv_ :: forall eff. GLuint -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

#### `vertexAttrib2f_`

    vertexAttrib2f_ :: forall eff. GLuint -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

#### `vertexAttrib2fv_`

    vertexAttrib2fv_ :: forall eff. GLuint -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

#### `vertexAttrib3f_`

    vertexAttrib3f_ :: forall eff. GLuint -> GLfloat -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

#### `vertexAttrib3fv_`

    vertexAttrib3fv_ :: forall eff. GLuint -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

#### `vertexAttrib4f_`

    vertexAttrib4f_ :: forall eff. GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

#### `vertexAttrib4fv_`

    vertexAttrib4fv_ :: forall eff. GLuint -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

#### `vertexAttribPointer_`

    vertexAttribPointer_ :: forall eff. GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> Eff (webgl :: WebGl | eff) Unit

#### `viewport_`

    viewport_ :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit


## Module Graphics.WebGLTexture

### Types

#### `InternalFormat`

    data InternalFormat
      = IF_ALPHA 
      | IF_LUMINANCE 
      | IF_LUMINANCE_ALPHA 
      | IF_RGB 
      | IF_RGBA 

#### `SymbolicParameter`

    data SymbolicParameter
      = PACK_ALIGNMENT 
      | UNPACK_ALIGNMENT 
      | UNPACK_FLIP_Y_WEBGL 
      | UNPACK_PREMULTIPLY_ALPHA_WEBGL 
      | UNPACK_COLORSPACE_CONVERSION_WEBGL 

#### `TargetType`

    data TargetType
      = TEXTURE_2D 
      | TEXTURE_CUBE_MAP_POSITIVE_X 
      | TEXTURE_CUBE_MAP_NEGATIVE_X 
      | TEXTURE_CUBE_MAP_POSITIVE_Y 
      | TEXTURE_CUBE_MAP_NEGATIVE_Y 
      | TEXTURE_CUBE_MAP_POSITIVE_Z 
      | TEXTURE_CUBE_MAP_NEGATIVE_Z 

#### `TexFilterSpec`

     texParNameToConst TEXTURE_MAX_ANISOTROPY_EXT = _TEXTURE_MAX_ANISOTROPY_EXT

    data TexFilterSpec
      = NEAREST 
      | LINEAR 
      | MIPMAP 

#### `TexParName`

    data TexParName
      = TEXTURE_MIN_FILTER 
      | TEXTURE_MAG_FILTER 
      | TEXTURE_WRAP_S 
      | TEXTURE_WRAP_T 
      | TEXTURE_MAX_ANISOTROPY_EXT 

#### `TexTarget`

    data TexTarget
      = TTEXTURE_2D 
      | TTEXTURE_CUBE_MAP 

#### `TextureType`

    data TextureType
      = UNSIGNED_BYTE 
      | RGBA 
      | FLOAT 
      | UNSIGNED_SHORT_5_6_5 
      | UNSIGNED_SHORT_4_4_4_4 
      | UNSIGNED_SHORT_5_5_5_1 

#### `WebGLTex`

    newtype WebGLTex
      = WebGLTex WebGLTexture


### Values

#### `activeTexture`

    activeTexture :: forall eff. Number -> Eff (webgl :: WebGl | eff) Unit

#### `bindTexture`

    bindTexture :: forall eff. TargetType -> WebGLTex -> EffWebGL eff Unit

#### `texture2DFor`

    texture2DFor :: forall a eff. String -> TexFilterSpec -> (WebGLTex -> EffWebGL eff a) -> EffWebGL eff Unit

#### `withTexture2D`

    withTexture2D :: forall eff typ. WebGLTex -> Number -> Uniform typ -> Number -> EffWebGL eff Unit