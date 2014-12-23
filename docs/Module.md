# Module Documentation

## Module Control.Monad.Eff.Alert

### Types

    data Alert :: !


### Values

    alert :: forall eff. String -> Eff (alert :: Alert | eff) Unit


## Module Control.Monad.Eff.WebGL

### Types

    type AttributeBinding = Tuple GLint Number

    type EffWebGL eff a = Eff (webgl :: WebGl | eff) a

    type MatrixBinding = WebGLUniformLocation

    data ShaderType where
      FragmentShader :: ShaderType
      VertexShader :: ShaderType

    type WebGLContext eff = { getCanvasHeight :: EffWebGL eff Number, getCanvasWidth :: EffWebGL eff Number, canvasName :: String }


### Values

    drawBuffer :: forall eff. WebGLProgram -> WebGLBuffer -> AttributeBinding -> Number -> Number -> EffWebGL eff Unit

    makeBuffer :: forall eff. [Number] -> Eff (webgl :: WebGl | eff) WebGLBuffer

    runWebGL :: forall a eff. String -> (String -> Eff eff a) -> (WebGLContext eff -> EffWebGL eff a) -> Eff eff a

    setMatrix :: forall eff. WebGLProgram -> MatrixBinding -> M4.Mat4 -> EffWebGL eff Unit

    vertexPointer :: forall eff. WebGLProgram -> AttributeBinding -> EffWebGL eff Unit

    withShaders :: forall a eff. String -> String -> [Tuple String Number] -> [String] -> (String -> EffWebGL eff a) -> (WebGLProgram -> [AttributeBinding] -> [MatrixBinding] -> EffWebGL eff a) -> EffWebGL eff a


## Module Control.Monad.Eff.WebGLRaw

### Types

    data ArrayBufferView :: *

    type FloatArray = Float32Array

    type GLbitfield = Number

    type GLboolean = Boolean

    type GLbyte = Number

    type GLclampf = Number

    type GLenum = Number

    type GLfloat = Number

    type GLint = Number

    type GLintptr = Number

    type GLshort = Number

    type GLsizei = Number

    type GLsizeiptr = Number

    type GLubyte = Number

    type GLuint = Number

    type GLushort = Number

    data HTMLImageElement :: *

    data HTMLVideoElement :: *

    data ImageData :: *

    data WebGLActiveInfo :: *

    data WebGLBuffer :: *

    data WebGLContextAttributes :: *

    data WebGLFramebuffer :: *

    data WebGLProgram :: *

    data WebGLRenderbuffer :: *

    data WebGLShader :: *

    data WebGLTexture :: *

    data WebGLUniformLocation :: *

    data WebGl :: !


### Values

    _ACTIVE_ATTRIBUTES :: Number

    _ACTIVE_ATTRIBUTE_MAX_LENGTH :: Number

    _ACTIVE_TEXTURE :: Number

    _ACTIVE_UNIFORMS :: Number

    _ACTIVE_UNIFORM_MAX_LENGTH :: Number

    _ALIASED_LINE_WIDTH_RANGE :: Number

    _ALIASED_POINT_SIZE_RANGE :: Number

    _ALPHA :: Number

    _ALPHA_BITS :: Number

    _ALWAYS :: Number

    _ARRAY_BUFFER :: Number

    _ARRAY_BUFFER_BINDING :: Number

    _ATTACHED_SHADERS :: Number

    _BACK :: Number

    _BLEND :: Number

    _BLEND_COLOR :: Number

    _BLEND_DST_ALPHA :: Number

    _BLEND_DST_RGB :: Number

    _BLEND_EQUATION :: Number

    _BLEND_EQUATION_ALPHA :: Number

    _BLEND_EQUATION_RGB :: Number

    _BLEND_SRC_ALPHA :: Number

    _BLEND_SRC_RGB :: Number

    _BLUE_BITS :: Number

    _BOOL :: Number

    _BOOL_VEC2 :: Number

    _BOOL_VEC3 :: Number

    _BOOL_VEC4 :: Number

    _BROWSER_DEFAULT_WEBGL :: Number

    _BUFFER_SIZE :: Number

    _BUFFER_USAGE :: Number

    _BYTE :: Number

    _CCW :: Number

    _CLAMP_TO_EDGE :: Number

    _COLOR_ATTACHMENT0 :: Number

    _COLOR_BUFFER_BIT :: Number

    _COLOR_CLEAR_VALUE :: Number

    _COLOR_WRITEMASK :: Number

    _COMPILE_STATUS :: Number

    _COMPRESSED_TEXTURE_FORMATS :: Number

    _CONSTANT_ALPHA :: Number

    _CONSTANT_COLOR :: Number

    _CONTEXT_LOST_WEBGL :: Number

    _CULL_FACE :: Number

    _CULL_FACE_MODE :: Number

    _CURRENT_PROGRAM :: Number

    _CURRENT_VERTEX_ATTRIB :: Number

    _CW :: Number

    _DECR :: Number

    _DECR_WRAP :: Number

    _DELETE_STATUS :: Number

    _DEPTH_ATTACHMENT :: Number

    _DEPTH_BITS :: Number

    _DEPTH_BUFFER_BIT :: Number

    _DEPTH_CLEAR_VALUE :: Number

    _DEPTH_COMPONENT :: Number

    _DEPTH_COMPONENT16 :: Number

    _DEPTH_FUNC :: Number

    _DEPTH_RANGE :: Number

    _DEPTH_STENCIL :: Number

    _DEPTH_STENCIL_ATTACHMENT :: Number

    _DEPTH_TEST :: Number

    _DEPTH_WRITEMASK :: Number

    _DITHER :: Number

    _DONT_CARE :: Number

    _DST_ALPHA :: Number

    _DST_COLOR :: Number

    _DYNAMIC_DRAW :: Number

    _ELEMENT_ARRAY_BUFFER :: Number

    _ELEMENT_ARRAY_BUFFER_BINDING :: Number

    _EQUAL :: Number

    _FASTEST :: Number

    _FLOAT :: Number

    _FLOAT_MAT2 :: Number

    _FLOAT_MAT3 :: Number

    _FLOAT_MAT4 :: Number

    _FLOAT_VEC2 :: Number

    _FLOAT_VEC3 :: Number

    _FLOAT_VEC4 :: Number

    _FRAGMENT_SHADER :: Number

    _FRAMEBUFFER :: Number

    _FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: Number

    _FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: Number

    _FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: Number

    _FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: Number

    _FRAMEBUFFER_BINDING :: Number

    _FRAMEBUFFER_COMPLETE :: Number

    _FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: Number

    _FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: Number

    _FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: Number

    _FRAMEBUFFER_UNSUPPORTED :: Number

    _FRONT :: Number

    _FRONT_AND_BACK :: Number

    _FRONT_FACE :: Number

    _FUNC_ADD :: Number

    _FUNC_REVERSE_SUBTRACT :: Number

    _FUNC_SUBTRACT :: Number

    _GENERATE_MIPMAP_HINT :: Number

    _GEQUAL :: Number

    _GREATER :: Number

    _GREEN_BITS :: Number

    _HIGH_FLOAT :: Number

    _HIGH_INT :: Number

    _INCR :: Number

    _INCR_WRAP :: Number

    _INFO_LOG_LENGTH :: Number

    _INT :: Number

    _INT_VEC2 :: Number

    _INT_VEC3 :: Number

    _INT_VEC4 :: Number

    _INVALID_ENUM :: Number

    _INVALID_FRAMEBUFFER_OPERATION :: Number

    _INVALID_OPERATION :: Number

    _INVALID_VALUE :: Number

    _INVERT :: Number

    _KEEP :: Number

    _LEQUAL :: Number

    _LESS :: Number

    _LINEAR :: Number

    _LINEAR_MIPMAP_LINEAR :: Number

    _LINEAR_MIPMAP_NEAREST :: Number

    _LINES :: Number

    _LINE_LOOP :: Number

    _LINE_STRIP :: Number

    _LINE_WIDTH :: Number

    _LINK_STATUS :: Number

    _LOW_FLOAT :: Number

    _LOW_INT :: Number

    _LUMINANCE :: Number

    _LUMINANCE_ALPHA :: Number

    _MAX_COMBINED_TEXTURE_IMAGE_UNITS :: Number

    _MAX_CUBE_MAP_TEXTURE_SIZE :: Number

    _MAX_FRAGMENT_UNIFORM_VECTORS :: Number

    _MAX_RENDERBUFFER_SIZE :: Number

    _MAX_TEXTURE_IMAGE_UNITS :: Number

    _MAX_TEXTURE_SIZE :: Number

    _MAX_VARYING_VECTORS :: Number

    _MAX_VERTEX_ATTRIBS :: Number

    _MAX_VERTEX_TEXTURE_IMAGE_UNITS :: Number

    _MAX_VERTEX_UNIFORM_VECTORS :: Number

    _MAX_VIEWPORT_DIMS :: Number

    _MEDIUM_FLOAT :: Number

    _MEDIUM_INT :: Number

    _MIRRORED_REPEAT :: Number

    _NEAREST :: Number

    _NEAREST_MIPMAP_LINEAR :: Number

    _NEAREST_MIPMAP_NEAREST :: Number

    _NEVER :: Number

    _NICEST :: Number

    _NONE :: Number

    _NOTEQUAL :: Number

    _NO_ERROR :: Number

    _NUM_COMPRESSED_TEXTURE_FORMATS :: Number

    _ONE :: Number

    _ONE_MINUS_CONSTANT_ALPHA :: Number

    _ONE_MINUS_CONSTANT_COLOR :: Number

    _ONE_MINUS_DST_ALPHA :: Number

    _ONE_MINUS_DST_COLOR :: Number

    _ONE_MINUS_SRC_ALPHA :: Number

    _ONE_MINUS_SRC_COLOR :: Number

    _OUT_OF_MEMORY :: Number

    _PACK_ALIGNMENT :: Number

    _POINTS :: Number

    _POLYGON_OFFSET_FACTOR :: Number

    _POLYGON_OFFSET_FILL :: Number

    _POLYGON_OFFSET_UNITS :: Number

    _RED_BITS :: Number

    _RENDERBUFFER :: Number

    _RENDERBUFFER_ALPHA_SIZE :: Number

    _RENDERBUFFER_BINDING :: Number

    _RENDERBUFFER_BLUE_SIZE :: Number

    _RENDERBUFFER_DEPTH_SIZE :: Number

    _RENDERBUFFER_GREEN_SIZE :: Number

    _RENDERBUFFER_HEIGHT :: Number

    _RENDERBUFFER_INTERNAL_FORMAT :: Number

    _RENDERBUFFER_RED_SIZE :: Number

    _RENDERBUFFER_STENCIL_SIZE :: Number

    _RENDERBUFFER_WIDTH :: Number

    _RENDERER :: Number

    _REPEAT :: Number

    _REPLACE :: Number

    _RGB :: Number

    _RGB565 :: Number

    _RGB5_A1 :: Number

    _RGBA :: Number

    _RGBA4 :: Number

    _SAMPLER_2D :: Number

    _SAMPLER_CUBE :: Number

    _SAMPLES :: Number

    _SAMPLE_ALPHA_TO_COVERAGE :: Number

    _SAMPLE_BUFFERS :: Number

    _SAMPLE_COVERAGE :: Number

    _SAMPLE_COVERAGE_INVERT :: Number

    _SAMPLE_COVERAGE_VALUE :: Number

    _SCISSOR_BOX :: Number

    _SCISSOR_TEST :: Number

    _SHADER_SOURCE_LENGTH :: Number

    _SHADER_TYPE :: Number

    _SHADING_LANGUAGE_VERSION :: Number

    _SHORT :: Number

    _SRC_ALPHA :: Number

    _SRC_ALPHA_SATURATE :: Number

    _SRC_COLOR :: Number

    _STATIC_DRAW :: Number

    _STENCIL_ATTACHMENT :: Number

    _STENCIL_BACK_FAIL :: Number

    _STENCIL_BACK_FUNC :: Number

    _STENCIL_BACK_PASS_DEPTH_FAIL :: Number

    _STENCIL_BACK_PASS_DEPTH_PASS :: Number

    _STENCIL_BACK_REF :: Number

    _STENCIL_BACK_VALUE_MASK :: Number

    _STENCIL_BACK_WRITEMASK :: Number

    _STENCIL_BITS :: Number

    _STENCIL_BUFFER_BIT :: Number

    _STENCIL_CLEAR_VALUE :: Number

    _STENCIL_FAIL :: Number

    _STENCIL_FUNC :: Number

    _STENCIL_INDEX :: Number

    _STENCIL_INDEX8 :: Number

    _STENCIL_PASS_DEPTH_FAIL :: Number

    _STENCIL_PASS_DEPTH_PASS :: Number

    _STENCIL_REF :: Number

    _STENCIL_TEST :: Number

    _STENCIL_VALUE_MASK :: Number

    _STENCIL_WRITEMASK :: Number

    _STREAM_DRAW :: Number

    _SUBPIXEL_BITS :: Number

    _TEXTURE :: Number

    _TEXTURE0 :: Number

    _TEXTURE1 :: Number

    _TEXTURE10 :: Number

    _TEXTURE11 :: Number

    _TEXTURE12 :: Number

    _TEXTURE13 :: Number

    _TEXTURE14 :: Number

    _TEXTURE15 :: Number

    _TEXTURE16 :: Number

    _TEXTURE17 :: Number

    _TEXTURE18 :: Number

    _TEXTURE19 :: Number

    _TEXTURE2 :: Number

    _TEXTURE20 :: Number

    _TEXTURE21 :: Number

    _TEXTURE22 :: Number

    _TEXTURE23 :: Number

    _TEXTURE24 :: Number

    _TEXTURE25 :: Number

    _TEXTURE26 :: Number

    _TEXTURE27 :: Number

    _TEXTURE28 :: Number

    _TEXTURE29 :: Number

    _TEXTURE3 :: Number

    _TEXTURE30 :: Number

    _TEXTURE31 :: Number

    _TEXTURE4 :: Number

    _TEXTURE5 :: Number

    _TEXTURE6 :: Number

    _TEXTURE7 :: Number

    _TEXTURE8 :: Number

    _TEXTURE9 :: Number

    _TEXTURE_2D :: Number

    _TEXTURE_BINDING_2D :: Number

    _TEXTURE_BINDING_CUBE_MAP :: Number

    _TEXTURE_CUBE_MAP :: Number

    _TEXTURE_CUBE_MAP_NEGATIVE_X :: Number

    _TEXTURE_CUBE_MAP_NEGATIVE_Y :: Number

    _TEXTURE_CUBE_MAP_NEGATIVE_Z :: Number

    _TEXTURE_CUBE_MAP_POSITIVE_X :: Number

    _TEXTURE_CUBE_MAP_POSITIVE_Y :: Number

    _TEXTURE_CUBE_MAP_POSITIVE_Z :: Number

    _TEXTURE_MAG_FILTER :: Number

    _TEXTURE_MIN_FILTER :: Number

    _TEXTURE_WRAP_S :: Number

    _TEXTURE_WRAP_T :: Number

    _TRIANGLES :: Number

    _TRIANGLE_FAN :: Number

    _TRIANGLE_STRIP :: Number

    _UNPACK_ALIGNMENT :: Number

    _UNPACK_COLORSPACE_CONVERSION_WEBGL :: Number

    _UNPACK_FLIP_Y_WEBGL :: Number

    _UNPACK_PREMULTIPLY_ALPHA_WEBGL :: Number

    _UNSIGNED_BYTE :: Number

    _UNSIGNED_INT :: Number

    _UNSIGNED_SHORT :: Number

    _UNSIGNED_SHORT_4_4_4_4 :: Number

    _UNSIGNED_SHORT_5_5_5_1 :: Number

    _UNSIGNED_SHORT_5_6_5 :: Number

    _VALIDATE_STATUS :: Number

    _VENDOR :: Number

    _VERSION :: Number

    _VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: Number

    _VERTEX_ATTRIB_ARRAY_ENABLED :: Number

    _VERTEX_ATTRIB_ARRAY_NORMALIZED :: Number

    _VERTEX_ATTRIB_ARRAY_POINTER :: Number

    _VERTEX_ATTRIB_ARRAY_SIZE :: Number

    _VERTEX_ATTRIB_ARRAY_STRIDE :: Number

    _VERTEX_ATTRIB_ARRAY_TYPE :: Number

    _VERTEX_SHADER :: Number

    _VIEWPORT :: Number

    _ZERO :: Number

    activeTexture :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

    attachShader :: forall eff. WebGLProgram -> WebGLShader -> Eff (webgl :: WebGl | eff) Unit

    bindAttribLocation :: forall eff. WebGLProgram -> GLuint -> String -> Eff (webgl :: WebGl | eff) Unit

    bindBuffer :: forall eff. GLenum -> WebGLBuffer -> Eff (webgl :: WebGl | eff) Unit

    bindFramebuffer :: forall eff. GLenum -> WebGLFramebuffer -> Eff (webgl :: WebGl | eff) Unit

    bindRenderbuffer :: forall eff. GLenum -> WebGLRenderbuffer -> Eff (webgl :: WebGl | eff) Unit

    bindTexture :: forall eff. GLenum -> WebGLTexture -> Eff (webgl :: WebGl | eff) Unit

    blendColor :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit

    blendEquation :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

    blendEquationSeparate :: forall eff. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit

    blendFunc :: forall eff. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit

    blendFuncSeparate :: forall eff. GLenum -> GLenum -> GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit

    bufferData :: forall eff. GLenum -> ArrayBuffer Float32 -> GLenum -> Eff (webgl :: WebGl | eff) Unit

    bufferSubData :: forall eff. GLenum -> GLintptr -> ArrayBuffer Float32 -> Eff (webgl :: WebGl | eff) Unit

    checkFramebufferStatus :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) GLenum

    clear :: forall eff. GLbitfield -> Eff (webgl :: WebGl | eff) Unit

    clearColor :: forall eff. GLclampf -> GLclampf -> GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit

    clearDepth :: forall eff. GLclampf -> Eff (webgl :: WebGl | eff) Unit

    clearStencil :: forall eff. GLint -> Eff (webgl :: WebGl | eff) Unit

    colorMask :: forall eff. GLboolean -> GLboolean -> GLboolean -> GLboolean -> Eff (webgl :: WebGl | eff) Unit

    compileShader :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) Unit

    copyTexImage2D :: forall eff. GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> Eff (webgl :: WebGl | eff) Unit

    copyTexSubImage2D :: forall eff. GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit

    createBuffer :: forall eff. Eff (webgl :: WebGl | eff) WebGLBuffer

    createFramebuffer :: forall eff. Eff (webgl :: WebGl | eff) WebGLFramebuffer

    createProgram :: forall eff. Eff (webgl :: WebGl | eff) WebGLProgram

    createRenderbuffer :: forall eff. Eff (webgl :: WebGl | eff) WebGLRenderbuffer

    createShader :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) WebGLShader

    createTexture :: forall eff. Eff (webgl :: WebGl | eff) WebGLTexture

    cullFace :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

    deleteBuffer :: forall eff. WebGLBuffer -> Eff (webgl :: WebGl | eff) Unit

    deleteFramebuffer :: forall eff. WebGLFramebuffer -> Eff (webgl :: WebGl | eff) Unit

    deleteProgram :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) Unit

    deleteRenderbuffer :: forall eff. WebGLRenderbuffer -> Eff (webgl :: WebGl | eff) Unit

    deleteShader :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) Unit

    deleteTexture :: forall eff. WebGLTexture -> Eff (webgl :: WebGl | eff) Unit

    depthFunc :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

    depthMask :: forall eff. GLboolean -> Eff (webgl :: WebGl | eff) Unit

    depthRange :: forall eff. GLclampf -> GLclampf -> Eff (webgl :: WebGl | eff) Unit

    detachShader :: forall eff. WebGLProgram -> WebGLShader -> Eff (webgl :: WebGl | eff) Unit

    disable :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

    disableVertexAttribArray :: forall eff. GLuint -> Eff (webgl :: WebGl | eff) Unit

    drawArrays :: forall eff. GLenum -> GLint -> GLsizei -> Eff (webgl :: WebGl | eff) Unit

    drawElements :: forall eff. GLenum -> GLsizei -> GLenum -> GLintptr -> Eff (webgl :: WebGl | eff) Unit

    enable :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

    enableVertexAttribArray :: forall eff. GLuint -> Eff (webgl :: WebGl | eff) Unit

    finish :: forall eff. Eff (webgl :: WebGl | eff) Unit

    flush :: forall eff. Eff (webgl :: WebGl | eff) Unit

    framebufferRenderbuffer :: forall eff. GLenum -> GLenum -> GLenum -> WebGLRenderbuffer -> Eff (webgl :: WebGl | eff) Unit

    framebufferTexture2D :: forall eff. GLenum -> GLenum -> GLenum -> WebGLTexture -> GLint -> Eff (webgl :: WebGl | eff) Unit

    frontFace :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

    generateMipmap :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) Unit

    getActiveAttrib :: forall eff. WebGLProgram -> GLuint -> Eff (webgl :: WebGl | eff) WebGLActiveInfo

    getActiveUniform :: forall eff. WebGLProgram -> GLuint -> Eff (webgl :: WebGl | eff) WebGLActiveInfo

    getAttachedShaders :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) WebGLShader

    getAttribLocation :: forall eff. WebGLProgram -> String -> Eff (webgl :: WebGl | eff) GLint

    getBufferParameter :: forall eff ret. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) ret

    getContextAttributes :: forall eff. Eff (webgl :: WebGl | eff) WebGLContextAttributes

    getError :: forall eff. Eff (webgl :: WebGl | eff) GLenum

    getExtension :: forall eff ret. String -> Eff (webgl :: WebGl | eff) ret

    getFramebufferAttachmentParameter :: forall eff ret. GLenum -> GLenum -> GLenum -> Eff (webgl :: WebGl | eff) ret

    getParameter :: forall eff ret. GLenum -> Eff (webgl :: WebGl | eff) ret

    getProgramInfoLog :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) String

    getProgramParameter :: forall eff ret. WebGLProgram -> GLenum -> Eff (webgl :: WebGl | eff) ret

    getRenderbufferParameter :: forall eff ret. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) ret

    getShaderInfoLog :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) String

    getShaderParameter :: forall eff ret. WebGLShader -> GLenum -> Eff (webgl :: WebGl | eff) ret

    getShaderSource :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) String

    getSupportedExtensions :: forall eff. Eff (webgl :: WebGl | eff) String

    getTexParameter :: forall eff ret. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) ret

    getUniform :: forall eff ret. WebGLProgram -> WebGLUniformLocation -> Eff (webgl :: WebGl | eff) ret

    getUniformLocation :: forall eff. WebGLProgram -> String -> Eff (webgl :: WebGl | eff) WebGLUniformLocation

    getVertexAttrib :: forall eff ret. GLuint -> GLenum -> Eff (webgl :: WebGl | eff) ret

    getVertexAttribOffset :: forall eff. GLuint -> GLenum -> Eff (webgl :: WebGl | eff) GLsizeiptr

    hint :: forall eff. GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit

    isBuffer :: forall eff. WebGLBuffer -> Eff (webgl :: WebGl | eff) GLboolean

    isContextLost :: forall eff. Eff (webgl :: WebGl | eff) Boolean

    isEnabled :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) GLboolean

    isFramebuffer :: forall eff. WebGLFramebuffer -> Eff (webgl :: WebGl | eff) GLboolean

    isProgram :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) GLboolean

    isRenderbuffer :: forall eff. WebGLRenderbuffer -> Eff (webgl :: WebGl | eff) GLboolean

    isShader :: forall eff. WebGLShader -> Eff (webgl :: WebGl | eff) GLboolean

    isTexture :: forall eff. WebGLTexture -> Eff (webgl :: WebGl | eff) GLboolean

    lineWidth :: forall eff. GLfloat -> Eff (webgl :: WebGl | eff) Unit

    linkProgram :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) Unit

    pixelStorei :: forall eff. GLenum -> GLint -> Eff (webgl :: WebGl | eff) Unit

    polygonOffset :: forall eff. GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

    readPixels :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> ArrayBufferView -> Eff (webgl :: WebGl | eff) Unit

    renderbufferStorage :: forall eff. GLenum -> GLenum -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit

    sampleCoverage :: forall eff. GLclampf -> GLboolean -> Eff (webgl :: WebGl | eff) Unit

    scissor :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit

    shaderSource :: forall eff. WebGLShader -> String -> Eff (webgl :: WebGl | eff) Unit

    stencilFunc :: forall eff. GLenum -> GLint -> GLuint -> Eff (webgl :: WebGl | eff) Unit

    stencilFuncSeparate :: forall eff. GLenum -> GLenum -> GLint -> GLuint -> Eff (webgl :: WebGl | eff) Unit

    stencilMask :: forall eff. GLuint -> Eff (webgl :: WebGl | eff) Unit

    stencilMaskSeparate :: forall eff. GLenum -> GLuint -> Eff (webgl :: WebGl | eff) Unit

    stencilOp :: forall eff. GLenum -> GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit

    stencilOpSeparate :: forall eff. GLenum -> GLenum -> GLenum -> GLenum -> Eff (webgl :: WebGl | eff) Unit

    texImage2D :: forall eff. GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> ArrayBufferView -> Eff (webgl :: WebGl | eff) Unit

    texParameterf :: forall eff. GLenum -> GLenum -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

    texParameteri :: forall eff. GLenum -> GLenum -> GLint -> Eff (webgl :: WebGl | eff) Unit

    texSubImage2D :: forall eff. GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> ArrayBufferView -> Eff (webgl :: WebGl | eff) Unit

    uniform1f :: forall eff. WebGLUniformLocation -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

    uniform1fv :: forall eff. WebGLUniformLocation -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

    uniform1i :: forall eff. WebGLUniformLocation -> GLint -> Eff (webgl :: WebGl | eff) Unit

    uniform1iv :: forall eff. WebGLUniformLocation -> Int32Array -> Eff (webgl :: WebGl | eff) Unit

    uniform2f :: forall eff. WebGLUniformLocation -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

    uniform2fv :: forall eff. WebGLUniformLocation -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

    uniform2i :: forall eff. WebGLUniformLocation -> GLint -> GLint -> Eff (webgl :: WebGl | eff) Unit

    uniform2iv :: forall eff. WebGLUniformLocation -> Int32Array -> Eff (webgl :: WebGl | eff) Unit

    uniform3f :: forall eff. WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

    uniform3fv :: forall eff. WebGLUniformLocation -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

    uniform3i :: forall eff. WebGLUniformLocation -> GLint -> GLint -> GLint -> Eff (webgl :: WebGl | eff) Unit

    uniform3iv :: forall eff. WebGLUniformLocation -> Int32Array -> Eff (webgl :: WebGl | eff) Unit

    uniform4f :: forall eff. WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

    uniform4fv :: forall eff. WebGLUniformLocation -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

    uniform4i :: forall eff. WebGLUniformLocation -> GLint -> GLint -> GLint -> GLint -> Eff (webgl :: WebGl | eff) Unit

    uniform4iv :: forall eff. WebGLUniformLocation -> Int32Array -> Eff (webgl :: WebGl | eff) Unit

    uniformMatrix2fv :: forall eff. WebGLUniformLocation -> GLboolean -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

    uniformMatrix3fv :: forall eff. WebGLUniformLocation -> GLboolean -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

    uniformMatrix4fv :: forall eff. WebGLUniformLocation -> GLboolean -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

    useProgram :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) Unit

    validateProgram :: forall eff. WebGLProgram -> Eff (webgl :: WebGl | eff) Unit

    vertexAttrib1f :: forall eff. GLuint -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

    vertexAttrib1fv :: forall eff. GLuint -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

    vertexAttrib2f :: forall eff. GLuint -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

    vertexAttrib2fv :: forall eff. GLuint -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

    vertexAttrib3f :: forall eff. GLuint -> GLfloat -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

    vertexAttrib3fv :: forall eff. GLuint -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

    vertexAttrib4f :: forall eff. GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Eff (webgl :: WebGl | eff) Unit

    vertexAttrib4fv :: forall eff. GLuint -> FloatArray -> Eff (webgl :: WebGl | eff) Unit

    vertexAttribPointer :: forall eff. GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> Eff (webgl :: WebGl | eff) Unit

    viewport :: forall eff. GLint -> GLint -> GLsizei -> GLsizei -> Eff (webgl :: WebGl | eff) Unit


## Module Data.Matrix4

### Types

    newtype Mat4 where
      Mat4 :: [Number] -> Mat4

    type Vec3N = V3.Vec3 Number


### Values

    identity :: Mat4

    inverseOrthonormal :: Mat4 -> Mat4

    makeBasis :: Vec3N -> Vec3N -> Vec3N -> Mat4

    makeFrustum :: Number -> Number -> Number -> Number -> Number -> Number -> Mat4

    makeLookAt :: Vec3N -> Vec3N -> Vec3N -> Mat4

    makeOrtho :: Number -> Number -> Number -> Number -> Number -> Number -> Mat4

    makeOrtho2D :: Number -> Number -> Number -> Number -> Mat4

    makePerspective :: Number -> Number -> Number -> Number -> Mat4

    makeRotate :: Number -> Vec3N -> Mat4

    makeScale :: Vec3N -> Mat4

    makeScale3 :: Number -> Number -> Number -> Mat4

    makeTranslate :: Vec3N -> Mat4

    makeTranslate3 :: Number -> Number -> Number -> Mat4

    mat4 :: [Number] -> Mat4

    mul :: Mat4 -> Mat4 -> Mat4

    mulAffine :: Mat4 -> Mat4 -> Mat4

    rotate :: Number -> Vec3N -> Mat4 -> Mat4

    scale :: Vec3N -> Mat4 -> Mat4

    scale3 :: Number -> Number -> Number -> Mat4 -> Mat4

    transform :: Mat4 -> Vec3N -> Vec3N

    translate :: Vec3N -> Mat4 -> Mat4

    translate3 :: Number -> Number -> Number -> Mat4 -> Mat4

    transpose :: Mat4 -> Mat4


## Module Data.TypedArray

### Types

    data ArrayBuffer :: * -> *

    data Float32

    type Float32Array = ArrayBuffer Float32

    data Float64

    type Float64Array = ArrayBuffer Float64

    data Int16

    type Int16Array = ArrayBuffer Int16

    data Int32

    type Int32Array = ArrayBuffer Int32

    data Int8

    type Int8Array = ArrayBuffer Int8

    data Uint16

    type Uint16Array = ArrayBuffer Uint16

    data Uint32

    type Uint32Array = ArrayBuffer Uint32

    data Uint8

    type Uint8Array = ArrayBuffer Uint8

    data Uint8Clamped

    type Uint8ClampedArray = ArrayBuffer Uint8Clamped


### Values

    (!!) :: forall a. ArrayBuffer a -> Number -> Maybe Number

    asArray :: forall a. ArrayBuffer a -> [Number]

    asFloat32Array :: [Number] -> ArrayBuffer Float32

    asFloat64Array :: [Number] -> ArrayBuffer Float64

    asInt16Array :: [Number] -> ArrayBuffer Int16

    asInt32Array :: [Number] -> ArrayBuffer Int32

    asInt8Array :: [Number] -> ArrayBuffer Int8

    asUint16Array :: [Number] -> ArrayBuffer Uint16

    asUint32Array :: [Number] -> ArrayBuffer Uint32

    asUint8Array :: [Number] -> ArrayBuffer Uint8

    asUint8ClampedArray :: [Number] -> ArrayBuffer Uint8ClampedArray

    byteLength :: forall a. ArrayBuffer a -> Number

    insertAt :: forall a. Number -> a -> ArrayBuffer a -> ArrayBuffer a

    length :: forall a. ArrayBuffer a -> Number

    unsafeIndex :: forall a. ArrayBuffer a -> Number -> Number


## Module Data.Vector

### Types

    newtype Vec s a where
      Vec :: [a] -> Vec s a


### Type Class Instances

    instance applyVec :: Apply (Vec s)

    instance eqVec :: (Eq a) => Eq (Vec s a)

    instance foldableVector :: Foldable (Vec s)

    instance functorVec :: Functor (Vec s)

    instance showVec :: (Show a) => Show (Vec s a)


### Values

    add :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a

    direction :: forall s. Vec s Number -> Vec s Number -> Vec s Number

    distance :: forall s. Vec s Number -> Vec s Number -> Number

    distanceSquared :: forall s. Vec s Number -> Vec s Number -> Number

    dot :: forall s. Vec s Number -> Vec s Number -> Number

    mult :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a

    normalize :: forall s. Vec s Number -> Vec s Number

    scale :: forall a s. (Num a) => a -> Vec s a -> Vec s a

    sub :: forall a s. (Num a) => Vec s a -> Vec s a -> Vec s a

    vlength :: forall s. Vec s Number -> Number

    vlengthSquared :: forall s. Vec s Number -> Number

    vnegate :: forall a s. (Num a) => Vec s a -> Vec s a


## Module Data.Vector2

### Types

    data Two

    type Vec2 = Vec Two


### Values

    getX :: forall a. Vec2 a -> a

    getY :: forall a. Vec2 a -> a

    i :: Vec2 Number

    j :: Vec2 Number

    setX :: forall a. a -> Vec2 a -> Vec2 a

    setY :: forall a. a -> Vec2 a -> Vec2 a

    vec2 :: forall a. a -> a -> Vec2 a

    vec2' :: forall a. [a] -> Vec2 a


## Module Data.Vector3

### Types

    data Three

    type Vec3 = Vec Three


### Values

    cross :: forall a. (Num a) => Vec3 a -> Vec3 a -> Vec3 a

    getX :: forall a. Vec3 a -> a

    getY :: forall a. Vec3 a -> a

    getZ :: forall a. Vec3 a -> a

    i :: Vec3 Number

    j :: Vec3 Number

    k :: Vec3 Number

    setX :: forall a. a -> Vec3 a -> Vec3 a

    setY :: forall a. a -> Vec3 a -> Vec3 a

    setZ :: forall a. a -> Vec3 a -> Vec3 a

    vec3 :: forall a. a -> a -> a -> Vec3 a

    vec3' :: forall a. [a] -> Vec3 a


## Module Data.Vector4

### Types

    data Four

    type Vec4 = Vec Four


### Values

    getU :: forall a. Vec4 a -> a

    getX :: forall a. Vec4 a -> a

    getY :: forall a. Vec4 a -> a

    getZ :: forall a. Vec4 a -> a

    i :: Vec4 Number

    j :: Vec4 Number

    k :: Vec4 Number

    l :: Vec4 Number

    setU :: forall a. a -> Vec4 a -> Vec4 a

    setX :: forall a. a -> Vec4 a -> Vec4 a

    setY :: forall a. a -> Vec4 a -> Vec4 a

    setZ :: forall a. a -> Vec4 a -> Vec4 a

    vec4 :: forall a. a -> a -> a -> a -> Vec4 a

    vec4' :: forall a. [a] -> Vec4 a


## Module Main

### Values

    fshaderSource :: String

    main :: Eff (alert :: Alert, trace :: Trace) Unit

    vshaderSource :: String