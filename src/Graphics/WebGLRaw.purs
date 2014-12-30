-- Auto generated: don't change manually, use purescript-webgl-generator to modify!!
module Graphics.WebGLRaw where

import Control.Monad.Eff
import Control.Monad.Eff.WebGL
import Data.TypedArray


type GLenum = Number
type GLboolean = Boolean
type GLbitfield = Number
type GLbyte = Number
type GLshort = Number
type GLint = Number
type GLsizei = Number
type GLintptr = Number
type GLsizeiptr = Number
type GLubyte = Number
type GLushort = Number
type GLuint = Number
type GLfloat = Number
type GLclampf = Number
type FloatArray = Float32Array

-- *TypeDecls
foreign import data WebGLContextAttributes :: *
foreign import data WebGLProgram :: *
foreign import data WebGLShader :: *
foreign import data WebGLBuffer :: *
foreign import data WebGLFramebuffer :: *
foreign import data WebGLRenderbuffer :: *
foreign import data WebGLTexture :: *
foreign import data WebGLActiveInfo :: *
foreign import data WebGLUniformLocation :: *
foreign import data ArrayBufferView :: *
foreign import data ImageData :: *
foreign import data HTMLImageElement :: *
foreign import data HTMLVideoElement :: *

-- *Constants
_DEPTH_BUFFER_BIT :: Number
_DEPTH_BUFFER_BIT = 256

_STENCIL_BUFFER_BIT :: Number
_STENCIL_BUFFER_BIT = 1024

_COLOR_BUFFER_BIT :: Number
_COLOR_BUFFER_BIT = 16384

_POINTS :: Number
_POINTS = 0

_LINES :: Number
_LINES = 1

_LINE_LOOP :: Number
_LINE_LOOP = 2

_LINE_STRIP :: Number
_LINE_STRIP = 3

_TRIANGLES :: Number
_TRIANGLES = 4

_TRIANGLE_STRIP :: Number
_TRIANGLE_STRIP = 5

_TRIANGLE_FAN :: Number
_TRIANGLE_FAN = 6

_ZERO :: Number
_ZERO = 0

_ONE :: Number
_ONE = 1

_SRC_COLOR :: Number
_SRC_COLOR = 768

_ONE_MINUS_SRC_COLOR :: Number
_ONE_MINUS_SRC_COLOR = 769

_SRC_ALPHA :: Number
_SRC_ALPHA = 770

_ONE_MINUS_SRC_ALPHA :: Number
_ONE_MINUS_SRC_ALPHA = 771

_DST_ALPHA :: Number
_DST_ALPHA = 772

_ONE_MINUS_DST_ALPHA :: Number
_ONE_MINUS_DST_ALPHA = 773

_DST_COLOR :: Number
_DST_COLOR = 774

_ONE_MINUS_DST_COLOR :: Number
_ONE_MINUS_DST_COLOR = 775

_SRC_ALPHA_SATURATE :: Number
_SRC_ALPHA_SATURATE = 776

_FUNC_ADD :: Number
_FUNC_ADD = 32774

_BLEND_EQUATION :: Number
_BLEND_EQUATION = 32777

_BLEND_EQUATION_RGB :: Number
_BLEND_EQUATION_RGB = 32777

_BLEND_EQUATION_ALPHA :: Number
_BLEND_EQUATION_ALPHA = 34877

_FUNC_SUBTRACT :: Number
_FUNC_SUBTRACT = 32778

_FUNC_REVERSE_SUBTRACT :: Number
_FUNC_REVERSE_SUBTRACT = 32779

_BLEND_DST_RGB :: Number
_BLEND_DST_RGB = 32968

_BLEND_SRC_RGB :: Number
_BLEND_SRC_RGB = 32969

_BLEND_DST_ALPHA :: Number
_BLEND_DST_ALPHA = 32970

_BLEND_SRC_ALPHA :: Number
_BLEND_SRC_ALPHA = 32971

_CONSTANT_COLOR :: Number
_CONSTANT_COLOR = 32769

_ONE_MINUS_CONSTANT_COLOR :: Number
_ONE_MINUS_CONSTANT_COLOR = 32770

_CONSTANT_ALPHA :: Number
_CONSTANT_ALPHA = 32771

_ONE_MINUS_CONSTANT_ALPHA :: Number
_ONE_MINUS_CONSTANT_ALPHA = 32772

_BLEND_COLOR :: Number
_BLEND_COLOR = 32773

_ARRAY_BUFFER :: Number
_ARRAY_BUFFER = 34962

_ELEMENT_ARRAY_BUFFER :: Number
_ELEMENT_ARRAY_BUFFER = 34963

_ARRAY_BUFFER_BINDING :: Number
_ARRAY_BUFFER_BINDING = 34964

_ELEMENT_ARRAY_BUFFER_BINDING :: Number
_ELEMENT_ARRAY_BUFFER_BINDING = 34965

_STREAM_DRAW :: Number
_STREAM_DRAW = 35040

_STATIC_DRAW :: Number
_STATIC_DRAW = 35044

_DYNAMIC_DRAW :: Number
_DYNAMIC_DRAW = 35048

_BUFFER_SIZE :: Number
_BUFFER_SIZE = 34660

_BUFFER_USAGE :: Number
_BUFFER_USAGE = 34661

_CURRENT_VERTEX_ATTRIB :: Number
_CURRENT_VERTEX_ATTRIB = 34342

_FRONT :: Number
_FRONT = 1028

_BACK :: Number
_BACK = 1029

_FRONT_AND_BACK :: Number
_FRONT_AND_BACK = 1032

_TEXTURE_2D :: Number
_TEXTURE_2D = 3553

_CULL_FACE :: Number
_CULL_FACE = 2884

_BLEND :: Number
_BLEND = 3042

_DITHER :: Number
_DITHER = 3024

_STENCIL_TEST :: Number
_STENCIL_TEST = 2960

_DEPTH_TEST :: Number
_DEPTH_TEST = 2929

_SCISSOR_TEST :: Number
_SCISSOR_TEST = 3089

_POLYGON_OFFSET_FILL :: Number
_POLYGON_OFFSET_FILL = 32823

_SAMPLE_ALPHA_TO_COVERAGE :: Number
_SAMPLE_ALPHA_TO_COVERAGE = 32926

_SAMPLE_COVERAGE :: Number
_SAMPLE_COVERAGE = 32928

_NO_ERROR :: Number
_NO_ERROR = 0

_INVALID_ENUM :: Number
_INVALID_ENUM = 1280

_INVALID_VALUE :: Number
_INVALID_VALUE = 1281

_INVALID_OPERATION :: Number
_INVALID_OPERATION = 1282

_OUT_OF_MEMORY :: Number
_OUT_OF_MEMORY = 1285

_CW :: Number
_CW = 2304

_CCW :: Number
_CCW = 2305

_LINE_WIDTH :: Number
_LINE_WIDTH = 2849

_ALIASED_POINT_SIZE_RANGE :: Number
_ALIASED_POINT_SIZE_RANGE = 33901

_ALIASED_LINE_WIDTH_RANGE :: Number
_ALIASED_LINE_WIDTH_RANGE = 33902

_CULL_FACE_MODE :: Number
_CULL_FACE_MODE = 2885

_FRONT_FACE :: Number
_FRONT_FACE = 2886

_DEPTH_RANGE :: Number
_DEPTH_RANGE = 2928

_DEPTH_WRITEMASK :: Number
_DEPTH_WRITEMASK = 2930

_DEPTH_CLEAR_VALUE :: Number
_DEPTH_CLEAR_VALUE = 2931

_DEPTH_FUNC :: Number
_DEPTH_FUNC = 2932

_STENCIL_CLEAR_VALUE :: Number
_STENCIL_CLEAR_VALUE = 2961

_STENCIL_FUNC :: Number
_STENCIL_FUNC = 2962

_STENCIL_FAIL :: Number
_STENCIL_FAIL = 2964

_STENCIL_PASS_DEPTH_FAIL :: Number
_STENCIL_PASS_DEPTH_FAIL = 2965

_STENCIL_PASS_DEPTH_PASS :: Number
_STENCIL_PASS_DEPTH_PASS = 2966

_STENCIL_REF :: Number
_STENCIL_REF = 2967

_STENCIL_VALUE_MASK :: Number
_STENCIL_VALUE_MASK = 2963

_STENCIL_WRITEMASK :: Number
_STENCIL_WRITEMASK = 2968

_STENCIL_BACK_FUNC :: Number
_STENCIL_BACK_FUNC = 34816

_STENCIL_BACK_FAIL :: Number
_STENCIL_BACK_FAIL = 34817

_STENCIL_BACK_PASS_DEPTH_FAIL :: Number
_STENCIL_BACK_PASS_DEPTH_FAIL = 34818

_STENCIL_BACK_PASS_DEPTH_PASS :: Number
_STENCIL_BACK_PASS_DEPTH_PASS = 34819

_STENCIL_BACK_REF :: Number
_STENCIL_BACK_REF = 36003

_STENCIL_BACK_VALUE_MASK :: Number
_STENCIL_BACK_VALUE_MASK = 36004

_STENCIL_BACK_WRITEMASK :: Number
_STENCIL_BACK_WRITEMASK = 36005

_VIEWPORT :: Number
_VIEWPORT = 2978

_SCISSOR_BOX :: Number
_SCISSOR_BOX = 3088

_COLOR_CLEAR_VALUE :: Number
_COLOR_CLEAR_VALUE = 3106

_COLOR_WRITEMASK :: Number
_COLOR_WRITEMASK = 3107

_UNPACK_ALIGNMENT :: Number
_UNPACK_ALIGNMENT = 3317

_PACK_ALIGNMENT :: Number
_PACK_ALIGNMENT = 3333

_MAX_TEXTURE_SIZE :: Number
_MAX_TEXTURE_SIZE = 3379

_MAX_VIEWPORT_DIMS :: Number
_MAX_VIEWPORT_DIMS = 3386

_SUBPIXEL_BITS :: Number
_SUBPIXEL_BITS = 3408

_RED_BITS :: Number
_RED_BITS = 3410

_GREEN_BITS :: Number
_GREEN_BITS = 3411

_BLUE_BITS :: Number
_BLUE_BITS = 3412

_ALPHA_BITS :: Number
_ALPHA_BITS = 3413

_DEPTH_BITS :: Number
_DEPTH_BITS = 3414

_STENCIL_BITS :: Number
_STENCIL_BITS = 3415

_POLYGON_OFFSET_UNITS :: Number
_POLYGON_OFFSET_UNITS = 10752

_POLYGON_OFFSET_FACTOR :: Number
_POLYGON_OFFSET_FACTOR = 32824

_TEXTURE_BINDING_2D :: Number
_TEXTURE_BINDING_2D = 32873

_SAMPLE_BUFFERS :: Number
_SAMPLE_BUFFERS = 32936

_SAMPLES :: Number
_SAMPLES = 32937

_SAMPLE_COVERAGE_VALUE :: Number
_SAMPLE_COVERAGE_VALUE = 32938

_SAMPLE_COVERAGE_INVERT :: Number
_SAMPLE_COVERAGE_INVERT = 32939

_NUM_COMPRESSED_TEXTURE_FORMATS :: Number
_NUM_COMPRESSED_TEXTURE_FORMATS = 34466

_COMPRESSED_TEXTURE_FORMATS :: Number
_COMPRESSED_TEXTURE_FORMATS = 34467

_DONT_CARE :: Number
_DONT_CARE = 4352

_FASTEST :: Number
_FASTEST = 4353

_NICEST :: Number
_NICEST = 4354

_GENERATE_MIPMAP_HINT :: Number
_GENERATE_MIPMAP_HINT = 33170

_BYTE :: Number
_BYTE = 5120

_UNSIGNED_BYTE :: Number
_UNSIGNED_BYTE = 5121

_SHORT :: Number
_SHORT = 5122

_UNSIGNED_SHORT :: Number
_UNSIGNED_SHORT = 5123

_INT :: Number
_INT = 5124

_UNSIGNED_INT :: Number
_UNSIGNED_INT = 5125

_FLOAT :: Number
_FLOAT = 5126

_DEPTH_COMPONENT :: Number
_DEPTH_COMPONENT = 6402

_ALPHA :: Number
_ALPHA = 6406

_RGB :: Number
_RGB = 6407

_RGBA :: Number
_RGBA = 6408

_LUMINANCE :: Number
_LUMINANCE = 6409

_LUMINANCE_ALPHA :: Number
_LUMINANCE_ALPHA = 6410

_UNSIGNED_SHORT_4_4_4_4 :: Number
_UNSIGNED_SHORT_4_4_4_4 = 32819

_UNSIGNED_SHORT_5_5_5_1 :: Number
_UNSIGNED_SHORT_5_5_5_1 = 32820

_UNSIGNED_SHORT_5_6_5 :: Number
_UNSIGNED_SHORT_5_6_5 = 33635

_FRAGMENT_SHADER :: Number
_FRAGMENT_SHADER = 35632

_VERTEX_SHADER :: Number
_VERTEX_SHADER = 35633

_MAX_VERTEX_ATTRIBS :: Number
_MAX_VERTEX_ATTRIBS = 34921

_MAX_VERTEX_UNIFORM_VECTORS :: Number
_MAX_VERTEX_UNIFORM_VECTORS = 36347

_MAX_VARYING_VECTORS :: Number
_MAX_VARYING_VECTORS = 36348

_MAX_COMBINED_TEXTURE_IMAGE_UNITS :: Number
_MAX_COMBINED_TEXTURE_IMAGE_UNITS = 35661

_MAX_VERTEX_TEXTURE_IMAGE_UNITS :: Number
_MAX_VERTEX_TEXTURE_IMAGE_UNITS = 35660

_MAX_TEXTURE_IMAGE_UNITS :: Number
_MAX_TEXTURE_IMAGE_UNITS = 34930

_MAX_FRAGMENT_UNIFORM_VECTORS :: Number
_MAX_FRAGMENT_UNIFORM_VECTORS = 36349

_SHADER_TYPE :: Number
_SHADER_TYPE = 35663

_DELETE_STATUS :: Number
_DELETE_STATUS = 35712

_LINK_STATUS :: Number
_LINK_STATUS = 35714

_VALIDATE_STATUS :: Number
_VALIDATE_STATUS = 35715

_ATTACHED_SHADERS :: Number
_ATTACHED_SHADERS = 35717

_ACTIVE_UNIFORMS :: Number
_ACTIVE_UNIFORMS = 35718

_ACTIVE_UNIFORM_MAX_LENGTH :: Number
_ACTIVE_UNIFORM_MAX_LENGTH = 35719

_ACTIVE_ATTRIBUTES :: Number
_ACTIVE_ATTRIBUTES = 35721

_ACTIVE_ATTRIBUTE_MAX_LENGTH :: Number
_ACTIVE_ATTRIBUTE_MAX_LENGTH = 35722

_SHADING_LANGUAGE_VERSION :: Number
_SHADING_LANGUAGE_VERSION = 35724

_CURRENT_PROGRAM :: Number
_CURRENT_PROGRAM = 35725

_NEVER :: Number
_NEVER = 512

_LESS :: Number
_LESS = 513

_EQUAL :: Number
_EQUAL = 514

_LEQUAL :: Number
_LEQUAL = 515

_GREATER :: Number
_GREATER = 516

_NOTEQUAL :: Number
_NOTEQUAL = 517

_GEQUAL :: Number
_GEQUAL = 518

_ALWAYS :: Number
_ALWAYS = 519

_KEEP :: Number
_KEEP = 7680

_REPLACE :: Number
_REPLACE = 7681

_INCR :: Number
_INCR = 7682

_DECR :: Number
_DECR = 7683

_INVERT :: Number
_INVERT = 5386

_INCR_WRAP :: Number
_INCR_WRAP = 34055

_DECR_WRAP :: Number
_DECR_WRAP = 34056

_VENDOR :: Number
_VENDOR = 7936

_RENDERER :: Number
_RENDERER = 7937

_VERSION :: Number
_VERSION = 7938

_NEAREST :: Number
_NEAREST = 9728

_LINEAR :: Number
_LINEAR = 9729

_NEAREST_MIPMAP_NEAREST :: Number
_NEAREST_MIPMAP_NEAREST = 9984

_LINEAR_MIPMAP_NEAREST :: Number
_LINEAR_MIPMAP_NEAREST = 9985

_NEAREST_MIPMAP_LINEAR :: Number
_NEAREST_MIPMAP_LINEAR = 9986

_LINEAR_MIPMAP_LINEAR :: Number
_LINEAR_MIPMAP_LINEAR = 9987

_TEXTURE_MAG_FILTER :: Number
_TEXTURE_MAG_FILTER = 10240

_TEXTURE_MIN_FILTER :: Number
_TEXTURE_MIN_FILTER = 10241

_TEXTURE_WRAP_S :: Number
_TEXTURE_WRAP_S = 10242

_TEXTURE_WRAP_T :: Number
_TEXTURE_WRAP_T = 10243

_TEXTURE :: Number
_TEXTURE = 5890

_TEXTURE_CUBE_MAP :: Number
_TEXTURE_CUBE_MAP = 34067

_TEXTURE_BINDING_CUBE_MAP :: Number
_TEXTURE_BINDING_CUBE_MAP = 34068

_TEXTURE_CUBE_MAP_POSITIVE_X :: Number
_TEXTURE_CUBE_MAP_POSITIVE_X = 34069

_TEXTURE_CUBE_MAP_NEGATIVE_X :: Number
_TEXTURE_CUBE_MAP_NEGATIVE_X = 34070

_TEXTURE_CUBE_MAP_POSITIVE_Y :: Number
_TEXTURE_CUBE_MAP_POSITIVE_Y = 34071

_TEXTURE_CUBE_MAP_NEGATIVE_Y :: Number
_TEXTURE_CUBE_MAP_NEGATIVE_Y = 34072

_TEXTURE_CUBE_MAP_POSITIVE_Z :: Number
_TEXTURE_CUBE_MAP_POSITIVE_Z = 34073

_TEXTURE_CUBE_MAP_NEGATIVE_Z :: Number
_TEXTURE_CUBE_MAP_NEGATIVE_Z = 34074

_MAX_CUBE_MAP_TEXTURE_SIZE :: Number
_MAX_CUBE_MAP_TEXTURE_SIZE = 34076

_TEXTURE0 :: Number
_TEXTURE0 = 33984

_TEXTURE1 :: Number
_TEXTURE1 = 33985

_TEXTURE2 :: Number
_TEXTURE2 = 33986

_TEXTURE3 :: Number
_TEXTURE3 = 33987

_TEXTURE4 :: Number
_TEXTURE4 = 33988

_TEXTURE5 :: Number
_TEXTURE5 = 33989

_TEXTURE6 :: Number
_TEXTURE6 = 33990

_TEXTURE7 :: Number
_TEXTURE7 = 33991

_TEXTURE8 :: Number
_TEXTURE8 = 33992

_TEXTURE9 :: Number
_TEXTURE9 = 33993

_TEXTURE10 :: Number
_TEXTURE10 = 33994

_TEXTURE11 :: Number
_TEXTURE11 = 33995

_TEXTURE12 :: Number
_TEXTURE12 = 33996

_TEXTURE13 :: Number
_TEXTURE13 = 33997

_TEXTURE14 :: Number
_TEXTURE14 = 33998

_TEXTURE15 :: Number
_TEXTURE15 = 33999

_TEXTURE16 :: Number
_TEXTURE16 = 34000

_TEXTURE17 :: Number
_TEXTURE17 = 34001

_TEXTURE18 :: Number
_TEXTURE18 = 34002

_TEXTURE19 :: Number
_TEXTURE19 = 34003

_TEXTURE20 :: Number
_TEXTURE20 = 34004

_TEXTURE21 :: Number
_TEXTURE21 = 34005

_TEXTURE22 :: Number
_TEXTURE22 = 34006

_TEXTURE23 :: Number
_TEXTURE23 = 34007

_TEXTURE24 :: Number
_TEXTURE24 = 34008

_TEXTURE25 :: Number
_TEXTURE25 = 34009

_TEXTURE26 :: Number
_TEXTURE26 = 34010

_TEXTURE27 :: Number
_TEXTURE27 = 34011

_TEXTURE28 :: Number
_TEXTURE28 = 34012

_TEXTURE29 :: Number
_TEXTURE29 = 34013

_TEXTURE30 :: Number
_TEXTURE30 = 34014

_TEXTURE31 :: Number
_TEXTURE31 = 34015

_ACTIVE_TEXTURE :: Number
_ACTIVE_TEXTURE = 34016

_REPEAT :: Number
_REPEAT = 10497

_CLAMP_TO_EDGE :: Number
_CLAMP_TO_EDGE = 33071

_MIRRORED_REPEAT :: Number
_MIRRORED_REPEAT = 33648

_FLOAT_VEC2 :: Number
_FLOAT_VEC2 = 35664

_FLOAT_VEC3 :: Number
_FLOAT_VEC3 = 35665

_FLOAT_VEC4 :: Number
_FLOAT_VEC4 = 35666

_INT_VEC2 :: Number
_INT_VEC2 = 35667

_INT_VEC3 :: Number
_INT_VEC3 = 35668

_INT_VEC4 :: Number
_INT_VEC4 = 35669

_BOOL :: Number
_BOOL = 35670

_BOOL_VEC2 :: Number
_BOOL_VEC2 = 35671

_BOOL_VEC3 :: Number
_BOOL_VEC3 = 35672

_BOOL_VEC4 :: Number
_BOOL_VEC4 = 35673

_FLOAT_MAT2 :: Number
_FLOAT_MAT2 = 35674

_FLOAT_MAT3 :: Number
_FLOAT_MAT3 = 35675

_FLOAT_MAT4 :: Number
_FLOAT_MAT4 = 35676

_SAMPLER_2D :: Number
_SAMPLER_2D = 35678

_SAMPLER_CUBE :: Number
_SAMPLER_CUBE = 35680

_VERTEX_ATTRIB_ARRAY_ENABLED :: Number
_VERTEX_ATTRIB_ARRAY_ENABLED = 34338

_VERTEX_ATTRIB_ARRAY_SIZE :: Number
_VERTEX_ATTRIB_ARRAY_SIZE = 34339

_VERTEX_ATTRIB_ARRAY_STRIDE :: Number
_VERTEX_ATTRIB_ARRAY_STRIDE = 34340

_VERTEX_ATTRIB_ARRAY_TYPE :: Number
_VERTEX_ATTRIB_ARRAY_TYPE = 34341

_VERTEX_ATTRIB_ARRAY_NORMALIZED :: Number
_VERTEX_ATTRIB_ARRAY_NORMALIZED = 34922

_VERTEX_ATTRIB_ARRAY_POINTER :: Number
_VERTEX_ATTRIB_ARRAY_POINTER = 34373

_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: Number
_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = 34975

_COMPILE_STATUS :: Number
_COMPILE_STATUS = 35713

_INFO_LOG_LENGTH :: Number
_INFO_LOG_LENGTH = 35716

_SHADER_SOURCE_LENGTH :: Number
_SHADER_SOURCE_LENGTH = 35720

_LOW_FLOAT :: Number
_LOW_FLOAT = 36336

_MEDIUM_FLOAT :: Number
_MEDIUM_FLOAT = 36337

_HIGH_FLOAT :: Number
_HIGH_FLOAT = 36338

_LOW_INT :: Number
_LOW_INT = 36339

_MEDIUM_INT :: Number
_MEDIUM_INT = 36340

_HIGH_INT :: Number
_HIGH_INT = 36341

_FRAMEBUFFER :: Number
_FRAMEBUFFER = 36160

_RENDERBUFFER :: Number
_RENDERBUFFER = 36161

_RGBA4 :: Number
_RGBA4 = 32854

_RGB5_A1 :: Number
_RGB5_A1 = 32855

_RGB565 :: Number
_RGB565 = 36194

_DEPTH_COMPONENT16 :: Number
_DEPTH_COMPONENT16 = 33189

_STENCIL_INDEX :: Number
_STENCIL_INDEX = 6401

_STENCIL_INDEX8 :: Number
_STENCIL_INDEX8 = 36168

_DEPTH_STENCIL :: Number
_DEPTH_STENCIL = 34041

_RENDERBUFFER_WIDTH :: Number
_RENDERBUFFER_WIDTH = 36162

_RENDERBUFFER_HEIGHT :: Number
_RENDERBUFFER_HEIGHT = 36163

_RENDERBUFFER_INTERNAL_FORMAT :: Number
_RENDERBUFFER_INTERNAL_FORMAT = 36164

_RENDERBUFFER_RED_SIZE :: Number
_RENDERBUFFER_RED_SIZE = 36176

_RENDERBUFFER_GREEN_SIZE :: Number
_RENDERBUFFER_GREEN_SIZE = 36177

_RENDERBUFFER_BLUE_SIZE :: Number
_RENDERBUFFER_BLUE_SIZE = 36178

_RENDERBUFFER_ALPHA_SIZE :: Number
_RENDERBUFFER_ALPHA_SIZE = 36179

_RENDERBUFFER_DEPTH_SIZE :: Number
_RENDERBUFFER_DEPTH_SIZE = 36180

_RENDERBUFFER_STENCIL_SIZE :: Number
_RENDERBUFFER_STENCIL_SIZE = 36181

_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: Number
_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = 36048

_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: Number
_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = 36049

_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: Number
_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = 36050

_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: Number
_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = 36051

_COLOR_ATTACHMENT0 :: Number
_COLOR_ATTACHMENT0 = 36064

_DEPTH_ATTACHMENT :: Number
_DEPTH_ATTACHMENT = 36096

_STENCIL_ATTACHMENT :: Number
_STENCIL_ATTACHMENT = 36128

_DEPTH_STENCIL_ATTACHMENT :: Number
_DEPTH_STENCIL_ATTACHMENT = 33306

_NONE :: Number
_NONE = 0

_FRAMEBUFFER_COMPLETE :: Number
_FRAMEBUFFER_COMPLETE = 36053

_FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: Number
_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = 36054

_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: Number
_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = 36055

_FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: Number
_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = 36057

_FRAMEBUFFER_UNSUPPORTED :: Number
_FRAMEBUFFER_UNSUPPORTED = 36061

_FRAMEBUFFER_BINDING :: Number
_FRAMEBUFFER_BINDING = 36006

_RENDERBUFFER_BINDING :: Number
_RENDERBUFFER_BINDING = 36007

_MAX_RENDERBUFFER_SIZE :: Number
_MAX_RENDERBUFFER_SIZE = 34024

_INVALID_FRAMEBUFFER_OPERATION :: Number
_INVALID_FRAMEBUFFER_OPERATION = 1286

_UNPACK_FLIP_Y_WEBGL :: Number
_UNPACK_FLIP_Y_WEBGL = 37440

_UNPACK_PREMULTIPLY_ALPHA_WEBGL :: Number
_UNPACK_PREMULTIPLY_ALPHA_WEBGL = 37441

_CONTEXT_LOST_WEBGL :: Number
_CONTEXT_LOST_WEBGL = 37442

_UNPACK_COLORSPACE_CONVERSION_WEBGL :: Number
_UNPACK_COLORSPACE_CONVERSION_WEBGL = 37443

_BROWSER_DEFAULT_WEBGL :: Number
_BROWSER_DEFAULT_WEBGL = 37444


-- *Methods
foreign import getContextAttributes_
  """function getContextAttributes_()
   {var res = window.gl.getContextAttributes();
    if (res === undefined){
      throw "Undefined in  getContextAttributes"};
    return res;};"""
    :: forall eff. (Eff (webgl :: WebGl | eff) WebGLContextAttributes)

foreign import isContextLost_
  """function isContextLost_()
   {var res = window.gl.isContextLost();
    if (res === undefined){
      throw "Undefined in  isContextLost"};
    return res;};"""
    :: forall eff. (Eff (webgl :: WebGl | eff) Boolean)

foreign import getSupportedExtensions_
  """function getSupportedExtensions_()
   {var res = window.gl.getSupportedExtensions();
    if (res === undefined){
      throw "Undefined in  getSupportedExtensions"};
    return res;};"""
    :: forall eff. (Eff (webgl :: WebGl | eff) String)

foreign import getExtension_
  """function getExtension_(name)
   {return function()
    {var res = window.gl.getExtension(name);
     if (res === undefined){
       throw "Undefined in  getExtension"};
     return res;};};"""
    :: forall eff ret. String -> (Eff (webgl :: WebGl | eff) ret)

foreign import activeTexture_
  """function activeTexture_(texture)
   {return function()
    {window.gl.activeTexture(texture);};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) Unit)

foreign import attachShader_
  """function attachShader_(program)
   {return function(shader)
    {return function()
     {window.gl.attachShader(program,shader);};};};"""
    :: forall eff. WebGLProgram->
                   WebGLShader
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bindAttribLocation_
  """function bindAttribLocation_(program)
   {return function(index)
    {return function(name)
     {return function()
      {window.gl.bindAttribLocation(program,index,name);};};};};"""
    :: forall eff. WebGLProgram->
                   GLuint->
                   String
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bindBuffer_
  """function bindBuffer_(target)
   {return function(buffer)
    {return function()
     {window.gl.bindBuffer(target,buffer);};};};"""
    :: forall eff. GLenum->
                   WebGLBuffer
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bindFramebuffer_
  """function bindFramebuffer_(target)
   {return function(framebuffer)
    {return function()
     {window.gl.bindFramebuffer(target,framebuffer);};};};"""
    :: forall eff. GLenum->
                   WebGLFramebuffer
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bindRenderbuffer_
  """function bindRenderbuffer_(target)
   {return function(renderbuffer)
    {return function()
     {window.gl.bindRenderbuffer(target,renderbuffer);};};};"""
    :: forall eff. GLenum->
                   WebGLRenderbuffer
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bindTexture_
  """function bindTexture_(target)
   {return function(texture)
    {return function()
     {window.gl.bindTexture(target,texture);};};};"""
    :: forall eff. GLenum->
                   WebGLTexture
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import blendColor_
  """function blendColor_(red)
   {return function(green)
    {return function(blue)
     {return function(alpha)
      {return function()
       {window.gl.blendColor(red,green,blue,alpha);};};};};};"""
    :: forall eff. GLclampf->
                   GLclampf->
                   GLclampf->
                   GLclampf
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import blendEquation_
  """function blendEquation_(mode)
   {return function()
    {window.gl.blendEquation(mode);};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) Unit)

foreign import blendEquationSeparate_
  """function blendEquationSeparate_(modeRGB)
   {return function(modeAlpha)
    {return function()
     {window.gl.blendEquationSeparate(modeRGB,modeAlpha);};};};"""
    :: forall eff. GLenum-> GLenum -> (Eff (webgl :: WebGl | eff) Unit)

foreign import blendFunc_
  """function blendFunc_(sfactor)
   {return function(dfactor)
    {return function()
     {window.gl.blendFunc(sfactor,dfactor);};};};"""
    :: forall eff. GLenum-> GLenum -> (Eff (webgl :: WebGl | eff) Unit)

foreign import blendFuncSeparate_
  """function blendFuncSeparate_(srcRGB)
   {return function(dstRGB)
    {return function(srcAlpha)
     {return function(dstAlpha)
      {return function()
       {window.gl.blendFuncSeparate(srcRGB,dstRGB,srcAlpha,dstAlpha);};};};};};"""
    :: forall eff. GLenum->
                   GLenum->
                   GLenum->
                   GLenum
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bufferData_
  """function bufferData_(target)
   {return function(data)
    {return function(usage)
     {return function()
      {window.gl.bufferData(target,data,usage);};};};};"""
    :: forall eff. GLenum->
                   ArrayBuffer Float32->
                   GLenum
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bufferSubData_
  """function bufferSubData_(target)
   {return function(offset)
    {return function(data)
     {return function()
      {window.gl.bufferSubData(target,offset,data);};};};};"""
    :: forall eff. GLenum->
                   GLintptr->
                   ArrayBuffer Float32
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import checkFramebufferStatus_
  """function checkFramebufferStatus_(target)
   {return function()
    {var res = window.gl.checkFramebufferStatus(target);
     if (res === undefined){
       throw "Undefined in  checkFramebufferStatus"};
     return res;};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) GLenum)

foreign import clear_
  """function clear_(mask)
   {return function()
    {window.gl.clear(mask);};};"""
    :: forall eff. GLbitfield -> (Eff (webgl :: WebGl | eff) Unit)

foreign import clearColor_
  """function clearColor_(red)
   {return function(green)
    {return function(blue)
     {return function(alpha)
      {return function()
       {window.gl.clearColor(red,green,blue,alpha);};};};};};"""
    :: forall eff. GLclampf->
                   GLclampf->
                   GLclampf->
                   GLclampf
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import clearDepth_
  """function clearDepth_(depth)
   {return function()
    {window.gl.clearDepth(depth);};};"""
    :: forall eff. GLclampf -> (Eff (webgl :: WebGl | eff) Unit)

foreign import clearStencil_
  """function clearStencil_(s)
   {return function()
    {window.gl.clearStencil(s);};};"""
    :: forall eff. GLint -> (Eff (webgl :: WebGl | eff) Unit)

foreign import colorMask_
  """function colorMask_(red)
   {return function(green)
    {return function(blue)
     {return function(alpha)
      {return function()
       {window.gl.colorMask(red,green,blue,alpha);};};};};};"""
    :: forall eff. GLboolean->
                   GLboolean->
                   GLboolean->
                   GLboolean
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import compileShader_
  """function compileShader_(shader)
   {return function()
    {window.gl.compileShader(shader);};};"""
    :: forall eff. WebGLShader -> (Eff (webgl :: WebGl | eff) Unit)

foreign import copyTexImage2D_
  """function copyTexImage2D_(target)
   {return function(level)
    {return function(internalformat)
     {return function(x)
      {return function(y)
       {return function(width)
        {return function(height)
         {return function(border)
          {return function()
           {window.gl.copyTexImage2D(target,level,internalformat,x,y,width,height,border);};};};};};};};};};"""
    :: forall eff. GLenum->
                   GLint->
                   GLenum->
                   GLint->
                   GLint->
                   GLsizei->
                   GLsizei->
                   GLint
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import copyTexSubImage2D_
  """function copyTexSubImage2D_(target)
   {return function(level)
    {return function(xoffset)
     {return function(yoffset)
      {return function(x)
       {return function(y)
        {return function(width)
         {return function(height)
          {return function()
           {window.gl.copyTexSubImage2D(target,level,xoffset,yoffset,x,y,width,height);};};};};};};};};};"""
    :: forall eff. GLenum->
                   GLint->
                   GLint->
                   GLint->
                   GLint->
                   GLint->
                   GLsizei->
                   GLsizei
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import createBuffer_
  """function createBuffer_()
   {var res = window.gl.createBuffer();
    if (res === undefined){
      throw "Undefined in  createBuffer"};
    return res;};"""
    :: forall eff. (Eff (webgl :: WebGl | eff) WebGLBuffer)

foreign import createFramebuffer_
  """function createFramebuffer_()
   {var res = window.gl.createFramebuffer();
    if (res === undefined){
      throw "Undefined in  createFramebuffer"};
    return res;};"""
    :: forall eff. (Eff (webgl :: WebGl | eff) WebGLFramebuffer)

foreign import createProgram_
  """function createProgram_()
   {var res = window.gl.createProgram();
    if (res === undefined){
      throw "Undefined in  createProgram"};
    return res;};"""
    :: forall eff. (Eff (webgl :: WebGl | eff) WebGLProgram)

foreign import createRenderbuffer_
  """function createRenderbuffer_()
   {var res = window.gl.createRenderbuffer();
    if (res === undefined){
      throw "Undefined in  createRenderbuffer"};
    return res;};"""
    :: forall eff. (Eff (webgl :: WebGl | eff) WebGLRenderbuffer)

foreign import createShader_
  """function createShader_(type)
   {return function()
    {var res = window.gl.createShader(type);
     if (res === undefined){
       throw "Undefined in  createShader"};
     return res;};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) WebGLShader)

foreign import createTexture_
  """function createTexture_()
   {var res = window.gl.createTexture();
    if (res === undefined){
      throw "Undefined in  createTexture"};
    return res;};"""
    :: forall eff. (Eff (webgl :: WebGl | eff) WebGLTexture)

foreign import cullFace_
  """function cullFace_(mode)
   {return function()
    {window.gl.cullFace(mode);};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) Unit)

foreign import deleteBuffer_
  """function deleteBuffer_(buffer)
   {return function()
    {window.gl.deleteBuffer(buffer);};};"""
    :: forall eff. WebGLBuffer -> (Eff (webgl :: WebGl | eff) Unit)

foreign import deleteFramebuffer_
  """function deleteFramebuffer_(framebuffer)
   {return function()
    {window.gl.deleteFramebuffer(framebuffer);};};"""
    :: forall eff. WebGLFramebuffer
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import deleteProgram_
  """function deleteProgram_(program)
   {return function()
    {window.gl.deleteProgram(program);};};"""
    :: forall eff. WebGLProgram -> (Eff (webgl :: WebGl | eff) Unit)

foreign import deleteRenderbuffer_
  """function deleteRenderbuffer_(renderbuffer)
   {return function()
    {window.gl.deleteRenderbuffer(renderbuffer);};};"""
    :: forall eff. WebGLRenderbuffer
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import deleteShader_
  """function deleteShader_(shader)
   {return function()
    {window.gl.deleteShader(shader);};};"""
    :: forall eff. WebGLShader -> (Eff (webgl :: WebGl | eff) Unit)

foreign import deleteTexture_
  """function deleteTexture_(texture)
   {return function()
    {window.gl.deleteTexture(texture);};};"""
    :: forall eff. WebGLTexture -> (Eff (webgl :: WebGl | eff) Unit)

foreign import depthFunc_
  """function depthFunc_(func)
   {return function()
    {window.gl.depthFunc(func);};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) Unit)

foreign import depthMask_
  """function depthMask_(flag)
   {return function()
    {window.gl.depthMask(flag);};};"""
    :: forall eff. GLboolean -> (Eff (webgl :: WebGl | eff) Unit)

foreign import depthRange_
  """function depthRange_(zNear)
   {return function(zFar)
    {return function()
     {window.gl.depthRange(zNear,zFar);};};};"""
    :: forall eff. GLclampf->
                   GLclampf
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import detachShader_
  """function detachShader_(program)
   {return function(shader)
    {return function()
     {window.gl.detachShader(program,shader);};};};"""
    :: forall eff. WebGLProgram->
                   WebGLShader
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import disable_
  """function disable_(cap)
   {return function()
    {window.gl.disable(cap);};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) Unit)

foreign import disableVertexAttribArray_
  """function disableVertexAttribArray_(index)
   {return function()
    {window.gl.disableVertexAttribArray(index);};};"""
    :: forall eff. GLuint -> (Eff (webgl :: WebGl | eff) Unit)

foreign import drawArrays_
  """function drawArrays_(mode)
   {return function(first)
    {return function(count)
     {return function()
      {window.gl.drawArrays(mode,first,count);};};};};"""
    :: forall eff. GLenum->
                   GLint->
                   GLsizei
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import drawElements_
  """function drawElements_(mode)
   {return function(count)
    {return function(type)
     {return function(offset)
      {return function()
       {window.gl.drawElements(mode,count,type,offset);};};};};};"""
    :: forall eff. GLenum->
                   GLsizei->
                   GLenum->
                   GLintptr
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import enable_
  """function enable_(cap)
   {return function()
    {window.gl.enable(cap);};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) Unit)

foreign import enableVertexAttribArray_
  """function enableVertexAttribArray_(index)
   {return function()
    {window.gl.enableVertexAttribArray(index);};};"""
    :: forall eff. GLuint -> (Eff (webgl :: WebGl | eff) Unit)

foreign import finish_
  """function finish_()
   {window.gl.finish();};"""
    :: forall eff. (Eff (webgl :: WebGl | eff) Unit)

foreign import flush_
  """function flush_()
   {window.gl.flush();};"""
    :: forall eff. (Eff (webgl :: WebGl | eff) Unit)

foreign import framebufferRenderbuffer_
  """function framebufferRenderbuffer_(target)
   {return function(attachment)
    {return function(renderbuffertarget)
     {return function(renderbuffer)
      {return function()
       {window.gl.framebufferRenderbuffer(target,attachment,renderbuffertarget,renderbuffer);};};};};};"""
    :: forall eff. GLenum->
                   GLenum->
                   GLenum->
                   WebGLRenderbuffer
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import framebufferTexture2D_
  """function framebufferTexture2D_(target)
   {return function(attachment)
    {return function(textarget)
     {return function(texture)
      {return function(level)
       {return function()
        {window.gl.framebufferTexture2D(target,attachment,textarget,texture,level);};};};};};};"""
    :: forall eff. GLenum->
                   GLenum->
                   GLenum->
                   WebGLTexture->
                   GLint
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import frontFace_
  """function frontFace_(mode)
   {return function()
    {window.gl.frontFace(mode);};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) Unit)

foreign import generateMipmap_
  """function generateMipmap_(target)
   {return function()
    {window.gl.generateMipmap(target);};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) Unit)

foreign import getActiveAttrib_
  """function getActiveAttrib_(program)
   {return function(index)
    {return function()
     {var res = window.gl.getActiveAttrib(program,index);
      if (res === undefined){
        throw "Undefined in  getActiveAttrib"};
      return res;};};};"""
    :: forall eff. WebGLProgram->
                   GLuint
                   -> (Eff (webgl :: WebGl | eff) WebGLActiveInfo)

foreign import getActiveUniform_
  """function getActiveUniform_(program)
   {return function(index)
    {return function()
     {var res = window.gl.getActiveUniform(program,index);
      if (res === undefined){
        throw "Undefined in  getActiveUniform"};
      return res;};};};"""
    :: forall eff. WebGLProgram->
                   GLuint
                   -> (Eff (webgl :: WebGl | eff) WebGLActiveInfo)

foreign import getAttachedShaders_
  """function getAttachedShaders_(program)
   {return function()
    {var res = window.gl.getAttachedShaders(program);
     if (res === undefined){
       throw "Undefined in  getAttachedShaders"};
     return res;};};"""
    :: forall eff. WebGLProgram
                   -> (Eff (webgl :: WebGl | eff) WebGLShader)

foreign import getAttribLocation_
  """function getAttribLocation_(program)
   {return function(name)
    {return function()
     {var res = window.gl.getAttribLocation(program,name);
      if (res === undefined){
        throw "Undefined in  getAttribLocation"};
      return res;};};};"""
    :: forall eff. WebGLProgram->
                   String
                   -> (Eff (webgl :: WebGl | eff) GLint)

foreign import getParameter_
  """function getParameter_(pname)
   {return function()
    {var res = window.gl.getParameter(pname);
     if (res === undefined){
       throw "Undefined in  getParameter"};
     return res;};};"""
    :: forall eff ret. GLenum -> (Eff (webgl :: WebGl | eff) ret)

foreign import getBufferParameter_
  """function getBufferParameter_(target)
   {return function(pname)
    {return function()
     {var res = window.gl.getBufferParameter(target,pname);
      if (res === undefined){
        throw "Undefined in  getBufferParameter"};
      return res;};};};"""
    :: forall eff ret. GLenum->
                       GLenum
                       -> (Eff (webgl :: WebGl | eff) ret)

foreign import getError_
  """function getError_()
   {var res = window.gl.getError();
    if (res === undefined){
      throw "Undefined in  getError"};
    return res;};"""
    :: forall eff. (Eff (webgl :: WebGl | eff) GLenum)

foreign import getFramebufferAttachmentParameter_
  """function getFramebufferAttachmentParameter_(target)
   {return function(attachment)
    {return function(pname)
     {return function()
      {var res = window.gl.getFramebufferAttachmentParameter(target,attachment,pname);
       if (res === undefined){
         throw "Undefined in  getFramebufferAttachmentParameter"};
       return res;};};};};"""
    :: forall eff ret. GLenum->
                       GLenum->
                       GLenum
                       -> (Eff (webgl :: WebGl | eff) ret)

foreign import getProgramParameter_
  """function getProgramParameter_(program)
   {return function(pname)
    {return function()
     {var res = window.gl.getProgramParameter(program,pname);
      if (res === undefined){
        throw "Undefined in  getProgramParameter"};
      return res;};};};"""
    :: forall eff ret. WebGLProgram->
                       GLenum
                       -> (Eff (webgl :: WebGl | eff) ret)

foreign import getProgramInfoLog_
  """function getProgramInfoLog_(program)
   {return function()
    {var res = window.gl.getProgramInfoLog(program);
     if (res === undefined){
       throw "Undefined in  getProgramInfoLog"};
     return res;};};"""
    :: forall eff. WebGLProgram -> (Eff (webgl :: WebGl | eff) String)

foreign import getRenderbufferParameter_
  """function getRenderbufferParameter_(target)
   {return function(pname)
    {return function()
     {var res = window.gl.getRenderbufferParameter(target,pname);
      if (res === undefined){
        throw "Undefined in  getRenderbufferParameter"};
      return res;};};};"""
    :: forall eff ret. GLenum->
                       GLenum
                       -> (Eff (webgl :: WebGl | eff) ret)

foreign import getShaderParameter_
  """function getShaderParameter_(shader)
   {return function(pname)
    {return function()
     {var res = window.gl.getShaderParameter(shader,pname);
      if (res === undefined){
        throw "Undefined in  getShaderParameter"};
      return res;};};};"""
    :: forall eff ret. WebGLShader->
                       GLenum
                       -> (Eff (webgl :: WebGl | eff) ret)

foreign import getShaderInfoLog_
  """function getShaderInfoLog_(shader)
   {return function()
    {var res = window.gl.getShaderInfoLog(shader);
     if (res === undefined){
       throw "Undefined in  getShaderInfoLog"};
     return res;};};"""
    :: forall eff. WebGLShader -> (Eff (webgl :: WebGl | eff) String)

foreign import getShaderSource_
  """function getShaderSource_(shader)
   {return function()
    {var res = window.gl.getShaderSource(shader);
     if (res === undefined){
       throw "Undefined in  getShaderSource"};
     return res;};};"""
    :: forall eff. WebGLShader -> (Eff (webgl :: WebGl | eff) String)

foreign import getTexParameter_
  """function getTexParameter_(target)
   {return function(pname)
    {return function()
     {var res = window.gl.getTexParameter(target,pname);
      if (res === undefined){
        throw "Undefined in  getTexParameter"};
      return res;};};};"""
    :: forall eff ret. GLenum->
                       GLenum
                       -> (Eff (webgl :: WebGl | eff) ret)

foreign import getUniform_
  """function getUniform_(program)
   {return function(location)
    {return function()
     {var res = window.gl.getUniform(program,location);
      if (res === undefined){
        throw "Undefined in  getUniform"};
      return res;};};};"""
    :: forall eff ret. WebGLProgram->
                       WebGLUniformLocation
                       -> (Eff (webgl :: WebGl | eff) ret)

foreign import getUniformLocation_
  """function getUniformLocation_(program)
   {return function(name)
    {return function()
     {var res = window.gl.getUniformLocation(program,name);
      if (res === undefined){
        throw "Undefined in  getUniformLocation"};
      return res;};};};"""
    :: forall eff. WebGLProgram->
                   String
                   -> (Eff (webgl :: WebGl | eff) WebGLUniformLocation)

foreign import getVertexAttrib_
  """function getVertexAttrib_(index)
   {return function(pname)
    {return function()
     {var res = window.gl.getVertexAttrib(index,pname);
      if (res === undefined){
        throw "Undefined in  getVertexAttrib"};
      return res;};};};"""
    :: forall eff ret. GLuint->
                       GLenum
                       -> (Eff (webgl :: WebGl | eff) ret)

foreign import getVertexAttribOffset_
  """function getVertexAttribOffset_(index)
   {return function(pname)
    {return function()
     {var res = window.gl.getVertexAttribOffset(index,pname);
      if (res === undefined){
        throw "Undefined in  getVertexAttribOffset"};
      return res;};};};"""
    :: forall eff. GLuint->
                   GLenum
                   -> (Eff (webgl :: WebGl | eff) GLsizeiptr)

foreign import hint_
  """function hint_(target)
   {return function(mode)
    {return function()
     {window.gl.hint(target,mode);};};};"""
    :: forall eff. GLenum-> GLenum -> (Eff (webgl :: WebGl | eff) Unit)

foreign import isBuffer_
  """function isBuffer_(buffer)
   {return function()
    {var res = window.gl.isBuffer(buffer);
     if (res === undefined){
       throw "Undefined in  isBuffer"};
     return res;};};"""
    :: forall eff. WebGLBuffer
                   -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import isEnabled_
  """function isEnabled_(cap)
   {return function()
    {var res = window.gl.isEnabled(cap);
     if (res === undefined){
       throw "Undefined in  isEnabled"};
     return res;};};"""
    :: forall eff. GLenum -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import isFramebuffer_
  """function isFramebuffer_(framebuffer)
   {return function()
    {var res = window.gl.isFramebuffer(framebuffer);
     if (res === undefined){
       throw "Undefined in  isFramebuffer"};
     return res;};};"""
    :: forall eff. WebGLFramebuffer
                   -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import isProgram_
  """function isProgram_(program)
   {return function()
    {var res = window.gl.isProgram(program);
     if (res === undefined){
       throw "Undefined in  isProgram"};
     return res;};};"""
    :: forall eff. WebGLProgram
                   -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import isRenderbuffer_
  """function isRenderbuffer_(renderbuffer)
   {return function()
    {var res = window.gl.isRenderbuffer(renderbuffer);
     if (res === undefined){
       throw "Undefined in  isRenderbuffer"};
     return res;};};"""
    :: forall eff. WebGLRenderbuffer
                   -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import isShader_
  """function isShader_(shader)
   {return function()
    {var res = window.gl.isShader(shader);
     if (res === undefined){
       throw "Undefined in  isShader"};
     return res;};};"""
    :: forall eff. WebGLShader
                   -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import isTexture_
  """function isTexture_(texture)
   {return function()
    {var res = window.gl.isTexture(texture);
     if (res === undefined){
       throw "Undefined in  isTexture"};
     return res;};};"""
    :: forall eff. WebGLTexture
                   -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import lineWidth_
  """function lineWidth_(width)
   {return function()
    {window.gl.lineWidth(width);};};"""
    :: forall eff. GLfloat -> (Eff (webgl :: WebGl | eff) Unit)

foreign import linkProgram_
  """function linkProgram_(program)
   {return function()
    {window.gl.linkProgram(program);};};"""
    :: forall eff. WebGLProgram -> (Eff (webgl :: WebGl | eff) Unit)

foreign import pixelStorei_
  """function pixelStorei_(pname)
   {return function(param)
    {return function()
     {window.gl.pixelStorei(pname,param);};};};"""
    :: forall eff. GLenum-> GLint -> (Eff (webgl :: WebGl | eff) Unit)

foreign import polygonOffset_
  """function polygonOffset_(factor)
   {return function(units)
    {return function()
     {window.gl.polygonOffset(factor,units);};};};"""
    :: forall eff. GLfloat->
                   GLfloat
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import readPixels_
  """function readPixels_(x)
   {return function(y)
    {return function(width)
     {return function(height)
      {return function(format)
       {return function(type)
        {return function(pixels)
         {return function()
          {window.gl.readPixels(x,y,width,height,format,type,pixels);};};};};};};};};"""
    :: forall eff. GLint->
                   GLint->
                   GLsizei->
                   GLsizei->
                   GLenum->
                   GLenum->
                   ArrayBufferView
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import renderbufferStorage_
  """function renderbufferStorage_(target)
   {return function(internalformat)
    {return function(width)
     {return function(height)
      {return function()
       {window.gl.renderbufferStorage(target,internalformat,width,height);};};};};};"""
    :: forall eff. GLenum->
                   GLenum->
                   GLsizei->
                   GLsizei
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import sampleCoverage_
  """function sampleCoverage_(value)
   {return function(invert)
    {return function()
     {window.gl.sampleCoverage(value,invert);};};};"""
    :: forall eff. GLclampf->
                   GLboolean
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import scissor_
  """function scissor_(x)
   {return function(y)
    {return function(width)
     {return function(height)
      {return function()
       {window.gl.scissor(x,y,width,height);};};};};};"""
    :: forall eff. GLint->
                   GLint->
                   GLsizei->
                   GLsizei
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import shaderSource_
  """function shaderSource_(shader)
   {return function(source)
    {return function()
     {window.gl.shaderSource(shader,source);};};};"""
    :: forall eff. WebGLShader->
                   String
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import stencilFunc_
  """function stencilFunc_(func)
   {return function(ref)
    {return function(mask)
     {return function()
      {window.gl.stencilFunc(func,ref,mask);};};};};"""
    :: forall eff. GLenum->
                   GLint->
                   GLuint
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import stencilFuncSeparate_
  """function stencilFuncSeparate_(face)
   {return function(func)
    {return function(ref)
     {return function(mask)
      {return function()
       {window.gl.stencilFuncSeparate(face,func,ref,mask);};};};};};"""
    :: forall eff. GLenum->
                   GLenum->
                   GLint->
                   GLuint
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import stencilMask_
  """function stencilMask_(mask)
   {return function()
    {window.gl.stencilMask(mask);};};"""
    :: forall eff. GLuint -> (Eff (webgl :: WebGl | eff) Unit)

foreign import stencilMaskSeparate_
  """function stencilMaskSeparate_(face)
   {return function(mask)
    {return function()
     {window.gl.stencilMaskSeparate(face,mask);};};};"""
    :: forall eff. GLenum-> GLuint -> (Eff (webgl :: WebGl | eff) Unit)

foreign import stencilOp_
  """function stencilOp_(fail)
   {return function(zfail)
    {return function(zpass)
     {return function()
      {window.gl.stencilOp(fail,zfail,zpass);};};};};"""
    :: forall eff. GLenum->
                   GLenum->
                   GLenum
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import stencilOpSeparate_
  """function stencilOpSeparate_(face)
   {return function(fail)
    {return function(zfail)
     {return function(zpass)
      {return function()
       {window.gl.stencilOpSeparate(face,fail,zfail,zpass);};};};};};"""
    :: forall eff. GLenum->
                   GLenum->
                   GLenum->
                   GLenum
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import texImage2D_
  """function texImage2D_(target)
   {return function(level)
    {return function(internalformat)
     {return function(width)
      {return function(height)
       {return function(border)
        {return function(format)
         {return function(type)
          {return function(pixels)
           {return function()
            {window.gl.texImage2D(target,level,internalformat,width,height,border,format,type,pixels);};};};};};};};};};};"""
    :: forall eff. GLenum->
                   GLint->
                   GLenum->
                   GLsizei->
                   GLsizei->
                   GLint->
                   GLenum->
                   GLenum->
                   ArrayBufferView
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import texParameterf_
  """function texParameterf_(target)
   {return function(pname)
    {return function(param)
     {return function()
      {window.gl.texParameterf(target,pname,param);};};};};"""
    :: forall eff. GLenum->
                   GLenum->
                   GLfloat
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import texParameteri_
  """function texParameteri_(target)
   {return function(pname)
    {return function(param)
     {return function()
      {window.gl.texParameteri(target,pname,param);};};};};"""
    :: forall eff. GLenum->
                   GLenum->
                   GLint
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import texSubImage2D_
  """function texSubImage2D_(target)
   {return function(level)
    {return function(xoffset)
     {return function(yoffset)
      {return function(width)
       {return function(height)
        {return function(format)
         {return function(type)
          {return function(pixels)
           {return function()
            {window.gl.texSubImage2D(target,level,xoffset,yoffset,width,height,format,type,pixels);};};};};};};};};};};"""
    :: forall eff. GLenum->
                   GLint->
                   GLint->
                   GLint->
                   GLsizei->
                   GLsizei->
                   GLenum->
                   GLenum->
                   ArrayBufferView
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform1f_
  """function uniform1f_(location)
   {return function(x)
    {return function()
     {window.gl.uniform1f(location,x);};};};"""
    :: forall eff. WebGLUniformLocation->
                   GLfloat
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform1fv_
  """function uniform1fv_(location)
   {return function(v)
    {return function()
     {window.gl.uniform1fv(location,v);};};};"""
    :: forall eff. WebGLUniformLocation->
                   FloatArray
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform1i_
  """function uniform1i_(location)
   {return function(x)
    {return function()
     {window.gl.uniform1i(location,x);};};};"""
    :: forall eff. WebGLUniformLocation->
                   GLint
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform1iv_
  """function uniform1iv_(location)
   {return function(v)
    {return function()
     {window.gl.uniform1iv(location,v);};};};"""
    :: forall eff. WebGLUniformLocation->
                   Int32Array
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform2f_
  """function uniform2f_(location)
   {return function(x)
    {return function(y)
     {return function()
      {window.gl.uniform2f(location,x,y);};};};};"""
    :: forall eff. WebGLUniformLocation->
                   GLfloat->
                   GLfloat
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform2fv_
  """function uniform2fv_(location)
   {return function(v)
    {return function()
     {window.gl.uniform2fv(location,v);};};};"""
    :: forall eff. WebGLUniformLocation->
                   FloatArray
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform2i_
  """function uniform2i_(location)
   {return function(x)
    {return function(y)
     {return function()
      {window.gl.uniform2i(location,x,y);};};};};"""
    :: forall eff. WebGLUniformLocation->
                   GLint->
                   GLint
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform2iv_
  """function uniform2iv_(location)
   {return function(v)
    {return function()
     {window.gl.uniform2iv(location,v);};};};"""
    :: forall eff. WebGLUniformLocation->
                   Int32Array
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform3f_
  """function uniform3f_(location)
   {return function(x)
    {return function(y)
     {return function(z)
      {return function()
       {window.gl.uniform3f(location,x,y,z);};};};};};"""
    :: forall eff. WebGLUniformLocation->
                   GLfloat->
                   GLfloat->
                   GLfloat
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform3fv_
  """function uniform3fv_(location)
   {return function(v)
    {return function()
     {window.gl.uniform3fv(location,v);};};};"""
    :: forall eff. WebGLUniformLocation->
                   FloatArray
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform3i_
  """function uniform3i_(location)
   {return function(x)
    {return function(y)
     {return function(z)
      {return function()
       {window.gl.uniform3i(location,x,y,z);};};};};};"""
    :: forall eff. WebGLUniformLocation->
                   GLint->
                   GLint->
                   GLint
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform3iv_
  """function uniform3iv_(location)
   {return function(v)
    {return function()
     {window.gl.uniform3iv(location,v);};};};"""
    :: forall eff. WebGLUniformLocation->
                   Int32Array
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform4f_
  """function uniform4f_(location)
   {return function(x)
    {return function(y)
     {return function(z)
      {return function(w)
       {return function()
        {window.gl.uniform4f(location,x,y,z,w);};};};};};};"""
    :: forall eff. WebGLUniformLocation->
                   GLfloat->
                   GLfloat->
                   GLfloat->
                   GLfloat
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform4fv_
  """function uniform4fv_(location)
   {return function(v)
    {return function()
     {window.gl.uniform4fv(location,v);};};};"""
    :: forall eff. WebGLUniformLocation->
                   FloatArray
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform4i_
  """function uniform4i_(location)
   {return function(x)
    {return function(y)
     {return function(z)
      {return function(w)
       {return function()
        {window.gl.uniform4i(location,x,y,z,w);};};};};};};"""
    :: forall eff. WebGLUniformLocation->
                   GLint->
                   GLint->
                   GLint->
                   GLint
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform4iv_
  """function uniform4iv_(location)
   {return function(v)
    {return function()
     {window.gl.uniform4iv(location,v);};};};"""
    :: forall eff. WebGLUniformLocation->
                   Int32Array
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniformMatrix2fv_
  """function uniformMatrix2fv_(location)
   {return function(transpose)
    {return function(value)
     {return function()
      {window.gl.uniformMatrix2fv(location,transpose,value);};};};};"""
    :: forall eff. WebGLUniformLocation->
                   GLboolean->
                   FloatArray
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniformMatrix3fv_
  """function uniformMatrix3fv_(location)
   {return function(transpose)
    {return function(value)
     {return function()
      {window.gl.uniformMatrix3fv(location,transpose,value);};};};};"""
    :: forall eff. WebGLUniformLocation->
                   GLboolean->
                   FloatArray
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniformMatrix4fv_
  """function uniformMatrix4fv_(location)
   {return function(transpose)
    {return function(value)
     {return function()
      {window.gl.uniformMatrix4fv(location,transpose,value);};};};};"""
    :: forall eff. WebGLUniformLocation->
                   GLboolean->
                   FloatArray
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import useProgram_
  """function useProgram_(program)
   {return function()
    {window.gl.useProgram(program);};};"""
    :: forall eff. WebGLProgram -> (Eff (webgl :: WebGl | eff) Unit)

foreign import validateProgram_
  """function validateProgram_(program)
   {return function()
    {window.gl.validateProgram(program);};};"""
    :: forall eff. WebGLProgram -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib1f_
  """function vertexAttrib1f_(indx)
   {return function(x)
    {return function()
     {window.gl.vertexAttrib1f(indx,x);};};};"""
    :: forall eff. GLuint->
                   GLfloat
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib1fv_
  """function vertexAttrib1fv_(indx)
   {return function(values)
    {return function()
     {window.gl.vertexAttrib1fv(indx,values);};};};"""
    :: forall eff. GLuint->
                   FloatArray
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib2f_
  """function vertexAttrib2f_(indx)
   {return function(x)
    {return function(y)
     {return function()
      {window.gl.vertexAttrib2f(indx,x,y);};};};};"""
    :: forall eff. GLuint->
                   GLfloat->
                   GLfloat
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib2fv_
  """function vertexAttrib2fv_(indx)
   {return function(values)
    {return function()
     {window.gl.vertexAttrib2fv(indx,values);};};};"""
    :: forall eff. GLuint->
                   FloatArray
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib3f_
  """function vertexAttrib3f_(indx)
   {return function(x)
    {return function(y)
     {return function(z)
      {return function()
       {window.gl.vertexAttrib3f(indx,x,y,z);};};};};};"""
    :: forall eff. GLuint->
                   GLfloat->
                   GLfloat->
                   GLfloat
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib3fv_
  """function vertexAttrib3fv_(indx)
   {return function(values)
    {return function()
     {window.gl.vertexAttrib3fv(indx,values);};};};"""
    :: forall eff. GLuint->
                   FloatArray
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib4f_
  """function vertexAttrib4f_(indx)
   {return function(x)
    {return function(y)
     {return function(z)
      {return function(w)
       {return function()
        {window.gl.vertexAttrib4f(indx,x,y,z,w);};};};};};};"""
    :: forall eff. GLuint->
                   GLfloat->
                   GLfloat->
                   GLfloat->
                   GLfloat
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib4fv_
  """function vertexAttrib4fv_(indx)
   {return function(values)
    {return function()
     {window.gl.vertexAttrib4fv(indx,values);};};};"""
    :: forall eff. GLuint->
                   FloatArray
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttribPointer_
  """function vertexAttribPointer_(indx)
   {return function(size)
    {return function(type)
     {return function(normalized)
      {return function(stride)
       {return function(offset)
        {return function()
         {window.gl.vertexAttribPointer(indx,size,type,normalized,stride,offset);};};};};};};};"""
    :: forall eff. GLuint->
                   GLint->
                   GLenum->
                   GLboolean->
                   GLsizei->
                   GLintptr
                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import viewport_
  """function viewport_(x)
   {return function(y)
    {return function(width)
     {return function(height)
      {return function()
       {window.gl.viewport(x,y,width,height);};};};};};"""
    :: forall eff. GLint->
                   GLint->
                   GLsizei->
                   GLsizei
                   -> (Eff (webgl :: WebGl | eff) Unit)
