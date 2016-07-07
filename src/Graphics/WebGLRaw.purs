-- Auto generated: don't change manually, use purescript-webgl-generator to modify!!
module Graphics.WebGLRaw where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (WebGl)
import Data.ArrayBuffer.Types (Int32Array, Float32Array)


type GLenum = Int
type GLboolean = Boolean
type GLbitfield = Int
type GLbyte = Int
type GLshort = Int
type GLint = Int
type GLsizei = Int
type GLintptr = Int
type GLsizeiptr = Int
type GLubyte = Int
type GLushort = Int
type GLuint = Int
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
_DEPTH_BUFFER_BIT :: Int
_DEPTH_BUFFER_BIT = 256

_STENCIL_BUFFER_BIT :: Int
_STENCIL_BUFFER_BIT = 1024

_COLOR_BUFFER_BIT :: Int
_COLOR_BUFFER_BIT = 16384

_POINTS :: Int
_POINTS = 0

_LINES :: Int
_LINES = 1

_LINE_LOOP :: Int
_LINE_LOOP = 2

_LINE_STRIP :: Int
_LINE_STRIP = 3

_TRIANGLES :: Int
_TRIANGLES = 4

_TRIANGLE_STRIP :: Int
_TRIANGLE_STRIP = 5

_TRIANGLE_FAN :: Int
_TRIANGLE_FAN = 6

_ZERO :: Int
_ZERO = 0

_ONE :: Int
_ONE = 1

_SRC_COLOR :: Int
_SRC_COLOR = 768

_ONE_MINUS_SRC_COLOR :: Int
_ONE_MINUS_SRC_COLOR = 769

_SRC_ALPHA :: Int
_SRC_ALPHA = 770

_ONE_MINUS_SRC_ALPHA :: Int
_ONE_MINUS_SRC_ALPHA = 771

_DST_ALPHA :: Int
_DST_ALPHA = 772

_ONE_MINUS_DST_ALPHA :: Int
_ONE_MINUS_DST_ALPHA = 773

_DST_COLOR :: Int
_DST_COLOR = 774

_ONE_MINUS_DST_COLOR :: Int
_ONE_MINUS_DST_COLOR = 775

_SRC_ALPHA_SATURATE :: Int
_SRC_ALPHA_SATURATE = 776

_FUNC_ADD :: Int
_FUNC_ADD = 32774

_BLEND_EQUATION :: Int
_BLEND_EQUATION = 32777

_BLEND_EQUATION_RGB :: Int
_BLEND_EQUATION_RGB = 32777

_BLEND_EQUATION_ALPHA :: Int
_BLEND_EQUATION_ALPHA = 34877

_FUNC_SUBTRACT :: Int
_FUNC_SUBTRACT = 32778

_FUNC_REVERSE_SUBTRACT :: Int
_FUNC_REVERSE_SUBTRACT = 32779

_BLEND_DST_RGB :: Int
_BLEND_DST_RGB = 32968

_BLEND_SRC_RGB :: Int
_BLEND_SRC_RGB = 32969

_BLEND_DST_ALPHA :: Int
_BLEND_DST_ALPHA = 32970

_BLEND_SRC_ALPHA :: Int
_BLEND_SRC_ALPHA = 32971

_CONSTANT_COLOR :: Int
_CONSTANT_COLOR = 32769

_ONE_MINUS_CONSTANT_COLOR :: Int
_ONE_MINUS_CONSTANT_COLOR = 32770

_CONSTANT_ALPHA :: Int
_CONSTANT_ALPHA = 32771

_ONE_MINUS_CONSTANT_ALPHA :: Int
_ONE_MINUS_CONSTANT_ALPHA = 32772

_BLEND_COLOR :: Int
_BLEND_COLOR = 32773

_ARRAY_BUFFER :: Int
_ARRAY_BUFFER = 34962

_ELEMENT_ARRAY_BUFFER :: Int
_ELEMENT_ARRAY_BUFFER = 34963

_ARRAY_BUFFER_BINDING :: Int
_ARRAY_BUFFER_BINDING = 34964

_ELEMENT_ARRAY_BUFFER_BINDING :: Int
_ELEMENT_ARRAY_BUFFER_BINDING = 34965

_STREAM_DRAW :: Int
_STREAM_DRAW = 35040

_STATIC_DRAW :: Int
_STATIC_DRAW = 35044

_DYNAMIC_DRAW :: Int
_DYNAMIC_DRAW = 35048

_BUFFER_SIZE :: Int
_BUFFER_SIZE = 34660

_BUFFER_USAGE :: Int
_BUFFER_USAGE = 34661

_CURRENT_VERTEX_ATTRIB :: Int
_CURRENT_VERTEX_ATTRIB = 34342

_FRONT :: Int
_FRONT = 1028

_BACK :: Int
_BACK = 1029

_FRONT_AND_BACK :: Int
_FRONT_AND_BACK = 1032

_TEXTURE_2D :: Int
_TEXTURE_2D = 3553

_CULL_FACE :: Int
_CULL_FACE = 2884

_BLEND :: Int
_BLEND = 3042

_DITHER :: Int
_DITHER = 3024

_STENCIL_TEST :: Int
_STENCIL_TEST = 2960

_DEPTH_TEST :: Int
_DEPTH_TEST = 2929

_SCISSOR_TEST :: Int
_SCISSOR_TEST = 3089

_POLYGON_OFFSET_FILL :: Int
_POLYGON_OFFSET_FILL = 32823

_SAMPLE_ALPHA_TO_COVERAGE :: Int
_SAMPLE_ALPHA_TO_COVERAGE = 32926

_SAMPLE_COVERAGE :: Int
_SAMPLE_COVERAGE = 32928

_NO_ERROR :: Int
_NO_ERROR = 0

_INVALID_ENUM :: Int
_INVALID_ENUM = 1280

_INVALID_VALUE :: Int
_INVALID_VALUE = 1281

_INVALID_OPERATION :: Int
_INVALID_OPERATION = 1282

_OUT_OF_MEMORY :: Int
_OUT_OF_MEMORY = 1285

_CW :: Int
_CW = 2304

_CCW :: Int
_CCW = 2305

_LINE_WIDTH :: Int
_LINE_WIDTH = 2849

_ALIASED_POINT_SIZE_RANGE :: Int
_ALIASED_POINT_SIZE_RANGE = 33901

_ALIASED_LINE_WIDTH_RANGE :: Int
_ALIASED_LINE_WIDTH_RANGE = 33902

_CULL_FACE_MODE :: Int
_CULL_FACE_MODE = 2885

_FRONT_FACE :: Int
_FRONT_FACE = 2886

_DEPTH_RANGE :: Int
_DEPTH_RANGE = 2928

_DEPTH_WRITEMASK :: Int
_DEPTH_WRITEMASK = 2930

_DEPTH_CLEAR_VALUE :: Int
_DEPTH_CLEAR_VALUE = 2931

_DEPTH_FUNC :: Int
_DEPTH_FUNC = 2932

_STENCIL_CLEAR_VALUE :: Int
_STENCIL_CLEAR_VALUE = 2961

_STENCIL_FUNC :: Int
_STENCIL_FUNC = 2962

_STENCIL_FAIL :: Int
_STENCIL_FAIL = 2964

_STENCIL_PASS_DEPTH_FAIL :: Int
_STENCIL_PASS_DEPTH_FAIL = 2965

_STENCIL_PASS_DEPTH_PASS :: Int
_STENCIL_PASS_DEPTH_PASS = 2966

_STENCIL_REF :: Int
_STENCIL_REF = 2967

_STENCIL_VALUE_MASK :: Int
_STENCIL_VALUE_MASK = 2963

_STENCIL_WRITEMASK :: Int
_STENCIL_WRITEMASK = 2968

_STENCIL_BACK_FUNC :: Int
_STENCIL_BACK_FUNC = 34816

_STENCIL_BACK_FAIL :: Int
_STENCIL_BACK_FAIL = 34817

_STENCIL_BACK_PASS_DEPTH_FAIL :: Int
_STENCIL_BACK_PASS_DEPTH_FAIL = 34818

_STENCIL_BACK_PASS_DEPTH_PASS :: Int
_STENCIL_BACK_PASS_DEPTH_PASS = 34819

_STENCIL_BACK_REF :: Int
_STENCIL_BACK_REF = 36003

_STENCIL_BACK_VALUE_MASK :: Int
_STENCIL_BACK_VALUE_MASK = 36004

_STENCIL_BACK_WRITEMASK :: Int
_STENCIL_BACK_WRITEMASK = 36005

_VIEWPORT :: Int
_VIEWPORT = 2978

_SCISSOR_BOX :: Int
_SCISSOR_BOX = 3088

_COLOR_CLEAR_VALUE :: Int
_COLOR_CLEAR_VALUE = 3106

_COLOR_WRITEMASK :: Int
_COLOR_WRITEMASK = 3107

_UNPACK_ALIGNMENT :: Int
_UNPACK_ALIGNMENT = 3317

_PACK_ALIGNMENT :: Int
_PACK_ALIGNMENT = 3333

_MAX_TEXTURE_SIZE :: Int
_MAX_TEXTURE_SIZE = 3379

_MAX_VIEWPORT_DIMS :: Int
_MAX_VIEWPORT_DIMS = 3386

_SUBPIXEL_BITS :: Int
_SUBPIXEL_BITS = 3408

_RED_BITS :: Int
_RED_BITS = 3410

_GREEN_BITS :: Int
_GREEN_BITS = 3411

_BLUE_BITS :: Int
_BLUE_BITS = 3412

_ALPHA_BITS :: Int
_ALPHA_BITS = 3413

_DEPTH_BITS :: Int
_DEPTH_BITS = 3414

_STENCIL_BITS :: Int
_STENCIL_BITS = 3415

_POLYGON_OFFSET_UNITS :: Int
_POLYGON_OFFSET_UNITS = 10752

_POLYGON_OFFSET_FACTOR :: Int
_POLYGON_OFFSET_FACTOR = 32824

_TEXTURE_BINDING_2D :: Int
_TEXTURE_BINDING_2D = 32873

_SAMPLE_BUFFERS :: Int
_SAMPLE_BUFFERS = 32936

_SAMPLES :: Int
_SAMPLES = 32937

_SAMPLE_COVERAGE_VALUE :: Int
_SAMPLE_COVERAGE_VALUE = 32938

_SAMPLE_COVERAGE_INVERT :: Int
_SAMPLE_COVERAGE_INVERT = 32939

_NUM_COMPRESSED_TEXTURE_FORMATS :: Int
_NUM_COMPRESSED_TEXTURE_FORMATS = 34466

_COMPRESSED_TEXTURE_FORMATS :: Int
_COMPRESSED_TEXTURE_FORMATS = 34467

_DONT_CARE :: Int
_DONT_CARE = 4352

_FASTEST :: Int
_FASTEST = 4353

_NICEST :: Int
_NICEST = 4354

_GENERATE_MIPMAP_HINT :: Int
_GENERATE_MIPMAP_HINT = 33170

_BYTE :: Int
_BYTE = 5120

_UNSIGNED_BYTE :: Int
_UNSIGNED_BYTE = 5121

_SHORT :: Int
_SHORT = 5122

_UNSIGNED_SHORT :: Int
_UNSIGNED_SHORT = 5123

_INT :: Int
_INT = 5124

_UNSIGNED_INT :: Int
_UNSIGNED_INT = 5125

_FLOAT :: Int
_FLOAT = 5126

_DEPTH_COMPONENT :: Int
_DEPTH_COMPONENT = 6402

_ALPHA :: Int
_ALPHA = 6406

_RGB :: Int
_RGB = 6407

_RGBA :: Int
_RGBA = 6408

_LUMINANCE :: Int
_LUMINANCE = 6409

_LUMINANCE_ALPHA :: Int
_LUMINANCE_ALPHA = 6410

_UNSIGNED_SHORT_4_4_4_4 :: Int
_UNSIGNED_SHORT_4_4_4_4 = 32819

_UNSIGNED_SHORT_5_5_5_1 :: Int
_UNSIGNED_SHORT_5_5_5_1 = 32820

_UNSIGNED_SHORT_5_6_5 :: Int
_UNSIGNED_SHORT_5_6_5 = 33635

_FRAGMENT_SHADER :: Int
_FRAGMENT_SHADER = 35632

_VERTEX_SHADER :: Int
_VERTEX_SHADER = 35633

_MAX_VERTEX_ATTRIBS :: Int
_MAX_VERTEX_ATTRIBS = 34921

_MAX_VERTEX_UNIFORM_VECTORS :: Int
_MAX_VERTEX_UNIFORM_VECTORS = 36347

_MAX_VARYING_VECTORS :: Int
_MAX_VARYING_VECTORS = 36348

_MAX_COMBINED_TEXTURE_IMAGE_UNITS :: Int
_MAX_COMBINED_TEXTURE_IMAGE_UNITS = 35661

_MAX_VERTEX_TEXTURE_IMAGE_UNITS :: Int
_MAX_VERTEX_TEXTURE_IMAGE_UNITS = 35660

_MAX_TEXTURE_IMAGE_UNITS :: Int
_MAX_TEXTURE_IMAGE_UNITS = 34930

_MAX_FRAGMENT_UNIFORM_VECTORS :: Int
_MAX_FRAGMENT_UNIFORM_VECTORS = 36349

_SHADER_TYPE :: Int
_SHADER_TYPE = 35663

_DELETE_STATUS :: Int
_DELETE_STATUS = 35712

_LINK_STATUS :: Int
_LINK_STATUS = 35714

_VALIDATE_STATUS :: Int
_VALIDATE_STATUS = 35715

_ATTACHED_SHADERS :: Int
_ATTACHED_SHADERS = 35717

_ACTIVE_UNIFORMS :: Int
_ACTIVE_UNIFORMS = 35718

_ACTIVE_UNIFORM_MAX_LENGTH :: Int
_ACTIVE_UNIFORM_MAX_LENGTH = 35719

_ACTIVE_ATTRIBUTES :: Int
_ACTIVE_ATTRIBUTES = 35721

_ACTIVE_ATTRIBUTE_MAX_LENGTH :: Int
_ACTIVE_ATTRIBUTE_MAX_LENGTH = 35722

_SHADING_LANGUAGE_VERSION :: Int
_SHADING_LANGUAGE_VERSION = 35724

_CURRENT_PROGRAM :: Int
_CURRENT_PROGRAM = 35725

_NEVER :: Int
_NEVER = 512

_LESS :: Int
_LESS = 513

_EQUAL :: Int
_EQUAL = 514

_LEQUAL :: Int
_LEQUAL = 515

_GREATER :: Int
_GREATER = 516

_NOTEQUAL :: Int
_NOTEQUAL = 517

_GEQUAL :: Int
_GEQUAL = 518

_ALWAYS :: Int
_ALWAYS = 519

_KEEP :: Int
_KEEP = 7680

_REPLACE :: Int
_REPLACE = 7681

_INCR :: Int
_INCR = 7682

_DECR :: Int
_DECR = 7683

_INVERT :: Int
_INVERT = 5386

_INCR_WRAP :: Int
_INCR_WRAP = 34055

_DECR_WRAP :: Int
_DECR_WRAP = 34056

_VENDOR :: Int
_VENDOR = 7936

_RENDERER :: Int
_RENDERER = 7937

_VERSION :: Int
_VERSION = 7938

_NEAREST :: Int
_NEAREST = 9728

_LINEAR :: Int
_LINEAR = 9729

_NEAREST_MIPMAP_NEAREST :: Int
_NEAREST_MIPMAP_NEAREST = 9984

_LINEAR_MIPMAP_NEAREST :: Int
_LINEAR_MIPMAP_NEAREST = 9985

_NEAREST_MIPMAP_LINEAR :: Int
_NEAREST_MIPMAP_LINEAR = 9986

_LINEAR_MIPMAP_LINEAR :: Int
_LINEAR_MIPMAP_LINEAR = 9987

_TEXTURE_MAG_FILTER :: Int
_TEXTURE_MAG_FILTER = 10240

_TEXTURE_MIN_FILTER :: Int
_TEXTURE_MIN_FILTER = 10241

_TEXTURE_WRAP_S :: Int
_TEXTURE_WRAP_S = 10242

_TEXTURE_WRAP_T :: Int
_TEXTURE_WRAP_T = 10243

_TEXTURE :: Int
_TEXTURE = 5890

_TEXTURE_CUBE_MAP :: Int
_TEXTURE_CUBE_MAP = 34067

_TEXTURE_BINDING_CUBE_MAP :: Int
_TEXTURE_BINDING_CUBE_MAP = 34068

_TEXTURE_CUBE_MAP_POSITIVE_X :: Int
_TEXTURE_CUBE_MAP_POSITIVE_X = 34069

_TEXTURE_CUBE_MAP_NEGATIVE_X :: Int
_TEXTURE_CUBE_MAP_NEGATIVE_X = 34070

_TEXTURE_CUBE_MAP_POSITIVE_Y :: Int
_TEXTURE_CUBE_MAP_POSITIVE_Y = 34071

_TEXTURE_CUBE_MAP_NEGATIVE_Y :: Int
_TEXTURE_CUBE_MAP_NEGATIVE_Y = 34072

_TEXTURE_CUBE_MAP_POSITIVE_Z :: Int
_TEXTURE_CUBE_MAP_POSITIVE_Z = 34073

_TEXTURE_CUBE_MAP_NEGATIVE_Z :: Int
_TEXTURE_CUBE_MAP_NEGATIVE_Z = 34074

_MAX_CUBE_MAP_TEXTURE_SIZE :: Int
_MAX_CUBE_MAP_TEXTURE_SIZE = 34076

_TEXTURE0 :: Int
_TEXTURE0 = 33984

_TEXTURE1 :: Int
_TEXTURE1 = 33985

_TEXTURE2 :: Int
_TEXTURE2 = 33986

_TEXTURE3 :: Int
_TEXTURE3 = 33987

_TEXTURE4 :: Int
_TEXTURE4 = 33988

_TEXTURE5 :: Int
_TEXTURE5 = 33989

_TEXTURE6 :: Int
_TEXTURE6 = 33990

_TEXTURE7 :: Int
_TEXTURE7 = 33991

_TEXTURE8 :: Int
_TEXTURE8 = 33992

_TEXTURE9 :: Int
_TEXTURE9 = 33993

_TEXTURE10 :: Int
_TEXTURE10 = 33994

_TEXTURE11 :: Int
_TEXTURE11 = 33995

_TEXTURE12 :: Int
_TEXTURE12 = 33996

_TEXTURE13 :: Int
_TEXTURE13 = 33997

_TEXTURE14 :: Int
_TEXTURE14 = 33998

_TEXTURE15 :: Int
_TEXTURE15 = 33999

_TEXTURE16 :: Int
_TEXTURE16 = 34000

_TEXTURE17 :: Int
_TEXTURE17 = 34001

_TEXTURE18 :: Int
_TEXTURE18 = 34002

_TEXTURE19 :: Int
_TEXTURE19 = 34003

_TEXTURE20 :: Int
_TEXTURE20 = 34004

_TEXTURE21 :: Int
_TEXTURE21 = 34005

_TEXTURE22 :: Int
_TEXTURE22 = 34006

_TEXTURE23 :: Int
_TEXTURE23 = 34007

_TEXTURE24 :: Int
_TEXTURE24 = 34008

_TEXTURE25 :: Int
_TEXTURE25 = 34009

_TEXTURE26 :: Int
_TEXTURE26 = 34010

_TEXTURE27 :: Int
_TEXTURE27 = 34011

_TEXTURE28 :: Int
_TEXTURE28 = 34012

_TEXTURE29 :: Int
_TEXTURE29 = 34013

_TEXTURE30 :: Int
_TEXTURE30 = 34014

_TEXTURE31 :: Int
_TEXTURE31 = 34015

_ACTIVE_TEXTURE :: Int
_ACTIVE_TEXTURE = 34016

_REPEAT :: Int
_REPEAT = 10497

_CLAMP_TO_EDGE :: Int
_CLAMP_TO_EDGE = 33071

_MIRRORED_REPEAT :: Int
_MIRRORED_REPEAT = 33648

_FLOAT_VEC2 :: Int
_FLOAT_VEC2 = 35664

_FLOAT_VEC3 :: Int
_FLOAT_VEC3 = 35665

_FLOAT_VEC4 :: Int
_FLOAT_VEC4 = 35666

_INT_VEC2 :: Int
_INT_VEC2 = 35667

_INT_VEC3 :: Int
_INT_VEC3 = 35668

_INT_VEC4 :: Int
_INT_VEC4 = 35669

_BOOL :: Int
_BOOL = 35670

_BOOL_VEC2 :: Int
_BOOL_VEC2 = 35671

_BOOL_VEC3 :: Int
_BOOL_VEC3 = 35672

_BOOL_VEC4 :: Int
_BOOL_VEC4 = 35673

_FLOAT_MAT2 :: Int
_FLOAT_MAT2 = 35674

_FLOAT_MAT3 :: Int
_FLOAT_MAT3 = 35675

_FLOAT_MAT4 :: Int
_FLOAT_MAT4 = 35676

_SAMPLER_2D :: Int
_SAMPLER_2D = 35678

_SAMPLER_CUBE :: Int
_SAMPLER_CUBE = 35680

_VERTEX_ATTRIB_ARRAY_ENABLED :: Int
_VERTEX_ATTRIB_ARRAY_ENABLED = 34338

_VERTEX_ATTRIB_ARRAY_SIZE :: Int
_VERTEX_ATTRIB_ARRAY_SIZE = 34339

_VERTEX_ATTRIB_ARRAY_STRIDE :: Int
_VERTEX_ATTRIB_ARRAY_STRIDE = 34340

_VERTEX_ATTRIB_ARRAY_TYPE :: Int
_VERTEX_ATTRIB_ARRAY_TYPE = 34341

_VERTEX_ATTRIB_ARRAY_NORMALIZED :: Int
_VERTEX_ATTRIB_ARRAY_NORMALIZED = 34922

_VERTEX_ATTRIB_ARRAY_POINTER :: Int
_VERTEX_ATTRIB_ARRAY_POINTER = 34373

_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING :: Int
_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = 34975

_COMPILE_STATUS :: Int
_COMPILE_STATUS = 35713

_INFO_LOG_LENGTH :: Int
_INFO_LOG_LENGTH = 35716

_SHADER_SOURCE_LENGTH :: Int
_SHADER_SOURCE_LENGTH = 35720

_LOW_FLOAT :: Int
_LOW_FLOAT = 36336

_MEDIUM_FLOAT :: Int
_MEDIUM_FLOAT = 36337

_HIGH_FLOAT :: Int
_HIGH_FLOAT = 36338

_LOW_INT :: Int
_LOW_INT = 36339

_MEDIUM_INT :: Int
_MEDIUM_INT = 36340

_HIGH_INT :: Int
_HIGH_INT = 36341

_FRAMEBUFFER :: Int
_FRAMEBUFFER = 36160

_RENDERBUFFER :: Int
_RENDERBUFFER = 36161

_RGBA4 :: Int
_RGBA4 = 32854

_RGB5_A1 :: Int
_RGB5_A1 = 32855

_RGB565 :: Int
_RGB565 = 36194

_DEPTH_COMPONENT16 :: Int
_DEPTH_COMPONENT16 = 33189

_STENCIL_INDEX :: Int
_STENCIL_INDEX = 6401

_STENCIL_INDEX8 :: Int
_STENCIL_INDEX8 = 36168

_DEPTH_STENCIL :: Int
_DEPTH_STENCIL = 34041

_RENDERBUFFER_WIDTH :: Int
_RENDERBUFFER_WIDTH = 36162

_RENDERBUFFER_HEIGHT :: Int
_RENDERBUFFER_HEIGHT = 36163

_RENDERBUFFER_INTERNAL_FORMAT :: Int
_RENDERBUFFER_INTERNAL_FORMAT = 36164

_RENDERBUFFER_RED_SIZE :: Int
_RENDERBUFFER_RED_SIZE = 36176

_RENDERBUFFER_GREEN_SIZE :: Int
_RENDERBUFFER_GREEN_SIZE = 36177

_RENDERBUFFER_BLUE_SIZE :: Int
_RENDERBUFFER_BLUE_SIZE = 36178

_RENDERBUFFER_ALPHA_SIZE :: Int
_RENDERBUFFER_ALPHA_SIZE = 36179

_RENDERBUFFER_DEPTH_SIZE :: Int
_RENDERBUFFER_DEPTH_SIZE = 36180

_RENDERBUFFER_STENCIL_SIZE :: Int
_RENDERBUFFER_STENCIL_SIZE = 36181

_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE :: Int
_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = 36048

_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME :: Int
_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = 36049

_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL :: Int
_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = 36050

_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE :: Int
_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = 36051

_COLOR_ATTACHMENT0 :: Int
_COLOR_ATTACHMENT0 = 36064

_DEPTH_ATTACHMENT :: Int
_DEPTH_ATTACHMENT = 36096

_STENCIL_ATTACHMENT :: Int
_STENCIL_ATTACHMENT = 36128

_DEPTH_STENCIL_ATTACHMENT :: Int
_DEPTH_STENCIL_ATTACHMENT = 33306

_NONE :: Int
_NONE = 0

_FRAMEBUFFER_COMPLETE :: Int
_FRAMEBUFFER_COMPLETE = 36053

_FRAMEBUFFER_INCOMPLETE_ATTACHMENT :: Int
_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = 36054

_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT :: Int
_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = 36055

_FRAMEBUFFER_INCOMPLETE_DIMENSIONS :: Int
_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = 36057

_FRAMEBUFFER_UNSUPPORTED :: Int
_FRAMEBUFFER_UNSUPPORTED = 36061

_FRAMEBUFFER_BINDING :: Int
_FRAMEBUFFER_BINDING = 36006

_RENDERBUFFER_BINDING :: Int
_RENDERBUFFER_BINDING = 36007

_MAX_RENDERBUFFER_SIZE :: Int
_MAX_RENDERBUFFER_SIZE = 34024

_INVALID_FRAMEBUFFER_OPERATION :: Int
_INVALID_FRAMEBUFFER_OPERATION = 1286

_UNPACK_FLIP_Y_WEBGL :: Int
_UNPACK_FLIP_Y_WEBGL = 37440

_UNPACK_PREMULTIPLY_ALPHA_WEBGL :: Int
_UNPACK_PREMULTIPLY_ALPHA_WEBGL = 37441

_CONTEXT_LOST_WEBGL :: Int
_CONTEXT_LOST_WEBGL = 37442

_UNPACK_COLORSPACE_CONVERSION_WEBGL :: Int
_UNPACK_COLORSPACE_CONVERSION_WEBGL = 37443

_BROWSER_DEFAULT_WEBGL :: Int
_BROWSER_DEFAULT_WEBGL = 37444


-- *Methods
foreign import getContextAttributes_:: forall eff. (Eff (webgl :: WebGl | eff) WebGLContextAttributes)

foreign import isContextLost_:: forall eff. (Eff (webgl :: WebGl | eff) Boolean)

foreign import getSupportedExtensions_:: forall eff. (Eff (webgl :: WebGl | eff) String)

foreign import getExtension_:: forall eff ret. String
                                               -> (Eff (webgl :: WebGl | eff) ret)

foreign import activeTexture_:: forall eff. GLenum
                                            -> (Eff (webgl :: WebGl | eff) Unit)

foreign import attachShader_:: forall eff. WebGLProgram->
                                           WebGLShader
                                           -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bindAttribLocation_:: forall eff. WebGLProgram->
                                                 GLuint->
                                                 String
                                                 -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bindBuffer_:: forall eff. GLenum->
                                         WebGLBuffer
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bindFramebuffer_:: forall eff. GLenum->
                                              WebGLFramebuffer
                                              -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bindRenderbuffer_:: forall eff. GLenum->
                                               WebGLRenderbuffer
                                               -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bindTexture_:: forall eff. GLenum->
                                          WebGLTexture
                                          -> (Eff (webgl :: WebGl | eff) Unit)

foreign import blendColor_:: forall eff. GLclampf->
                                         GLclampf->
                                         GLclampf->
                                         GLclampf
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import blendEquation_:: forall eff. GLenum
                                            -> (Eff (webgl :: WebGl | eff) Unit)

foreign import blendEquationSeparate_:: forall eff. GLenum->
                                                    GLenum
                                                    -> (Eff (webgl :: WebGl | eff) Unit)

foreign import blendFunc_:: forall eff. GLenum->
                                        GLenum
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import blendFuncSeparate_:: forall eff. GLenum->
                                                GLenum->
                                                GLenum->
                                                GLenum
                                                -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bufferData_:: forall eff. GLenum->
                                         Float32Array->
                                         GLenum
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import bufferSubData_:: forall eff. GLenum->
                                            GLintptr->
                                            Float32Array
                                            -> (Eff (webgl :: WebGl | eff) Unit)

foreign import checkFramebufferStatus_:: forall eff. GLenum
                                                     -> (Eff (webgl :: WebGl | eff) GLenum)

foreign import clear_:: forall eff. GLbitfield
                                    -> (Eff (webgl :: WebGl | eff) Unit)

foreign import clearColor_:: forall eff. GLclampf->
                                         GLclampf->
                                         GLclampf->
                                         GLclampf
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import clearDepth_:: forall eff. GLclampf
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import clearStencil_:: forall eff. GLint
                                           -> (Eff (webgl :: WebGl | eff) Unit)

foreign import colorMask_:: forall eff. GLboolean->
                                        GLboolean->
                                        GLboolean->
                                        GLboolean
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import compileShader_:: forall eff. WebGLShader
                                            -> (Eff (webgl :: WebGl | eff) Unit)

foreign import copyTexImage2D_:: forall eff. GLenum->
                                             GLint->
                                             GLenum->
                                             GLint->
                                             GLint->
                                             GLsizei->
                                             GLsizei->
                                             GLint
                                             -> (Eff (webgl :: WebGl | eff) Unit)

foreign import copyTexSubImage2D_:: forall eff. GLenum->
                                                GLint->
                                                GLint->
                                                GLint->
                                                GLint->
                                                GLint->
                                                GLsizei->
                                                GLsizei
                                                -> (Eff (webgl :: WebGl | eff) Unit)

foreign import createBuffer_:: forall eff. (Eff (webgl :: WebGl | eff) WebGLBuffer)

foreign import createFramebuffer_:: forall eff. (Eff (webgl :: WebGl | eff) WebGLFramebuffer)

foreign import createProgram_:: forall eff. (Eff (webgl :: WebGl | eff) WebGLProgram)

foreign import createRenderbuffer_:: forall eff. (Eff (webgl :: WebGl | eff) WebGLRenderbuffer)

foreign import createShader_:: forall eff. GLenum
                                           -> (Eff (webgl :: WebGl | eff) WebGLShader)

foreign import createTexture_:: forall eff. (Eff (webgl :: WebGl | eff) WebGLTexture)

foreign import cullFace_:: forall eff. GLenum
                                       -> (Eff (webgl :: WebGl | eff) Unit)

foreign import deleteBuffer_:: forall eff. WebGLBuffer
                                           -> (Eff (webgl :: WebGl | eff) Unit)

foreign import deleteFramebuffer_:: forall eff. WebGLFramebuffer
                                                -> (Eff (webgl :: WebGl | eff) Unit)

foreign import deleteProgram_:: forall eff. WebGLProgram
                                            -> (Eff (webgl :: WebGl | eff) Unit)

foreign import deleteRenderbuffer_:: forall eff. WebGLRenderbuffer
                                                 -> (Eff (webgl :: WebGl | eff) Unit)

foreign import deleteShader_:: forall eff. WebGLShader
                                           -> (Eff (webgl :: WebGl | eff) Unit)

foreign import deleteTexture_:: forall eff. WebGLTexture
                                            -> (Eff (webgl :: WebGl | eff) Unit)

foreign import depthFunc_:: forall eff. GLenum
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import depthMask_:: forall eff. GLboolean
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import depthRange_:: forall eff. GLclampf->
                                         GLclampf
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import detachShader_:: forall eff. WebGLProgram->
                                           WebGLShader
                                           -> (Eff (webgl :: WebGl | eff) Unit)

foreign import disable_:: forall eff. GLenum
                                      -> (Eff (webgl :: WebGl | eff) Unit)

foreign import disableVertexAttribArray_:: forall eff. GLuint
                                                       -> (Eff (webgl :: WebGl | eff) Unit)

foreign import drawArrays_:: forall eff. GLenum->
                                         GLint->
                                         GLsizei
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import drawElements_:: forall eff. GLenum->
                                           GLsizei->
                                           GLenum->
                                           GLintptr
                                           -> (Eff (webgl :: WebGl | eff) Unit)

foreign import enable_:: forall eff. GLenum
                                     -> (Eff (webgl :: WebGl | eff) Unit)

foreign import enableVertexAttribArray_:: forall eff. GLuint
                                                      -> (Eff (webgl :: WebGl | eff) Unit)

foreign import finish_:: forall eff. (Eff (webgl :: WebGl | eff) Unit)

foreign import flush_:: forall eff. (Eff (webgl :: WebGl | eff) Unit)

foreign import framebufferRenderbuffer_:: forall eff. GLenum->
                                                      GLenum->
                                                      GLenum->
                                                      WebGLRenderbuffer
                                                      -> (Eff (webgl :: WebGl | eff) Unit)

foreign import framebufferTexture2D_:: forall eff. GLenum->
                                                   GLenum->
                                                   GLenum->
                                                   WebGLTexture->
                                                   GLint
                                                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import frontFace_:: forall eff. GLenum
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import generateMipmap_:: forall eff. GLenum
                                             -> (Eff (webgl :: WebGl | eff) Unit)

foreign import getActiveAttrib_:: forall eff. WebGLProgram->
                                              GLuint
                                              -> (Eff (webgl :: WebGl | eff) WebGLActiveInfo)

foreign import getActiveUniform_:: forall eff. WebGLProgram->
                                               GLuint
                                               -> (Eff (webgl :: WebGl | eff) WebGLActiveInfo)

foreign import getAttachedShaders_:: forall eff. WebGLProgram
                                                 -> (Eff (webgl :: WebGl | eff) WebGLShader)

foreign import getAttribLocation_:: forall eff. WebGLProgram->
                                                String
                                                -> (Eff (webgl :: WebGl | eff) GLint)

foreign import getParameter_:: forall eff ret. GLenum
                                               -> (Eff (webgl :: WebGl | eff) ret)

foreign import getBufferParameter_:: forall eff ret. GLenum->
                                                     GLenum
                                                     -> (Eff (webgl :: WebGl | eff) ret)

foreign import getError_:: forall eff. (Eff (webgl :: WebGl | eff) GLenum)

foreign import getFramebufferAttachmentParameter_:: forall eff ret. GLenum->
                                                                    GLenum->
                                                                    GLenum
                                                                    -> (Eff (webgl :: WebGl | eff) ret)

foreign import getProgramParameter_:: forall eff ret. WebGLProgram->
                                                      GLenum
                                                      -> (Eff (webgl :: WebGl | eff) ret)

foreign import getProgramInfoLog_:: forall eff. WebGLProgram
                                                -> (Eff (webgl :: WebGl | eff) String)

foreign import getRenderbufferParameter_:: forall eff ret. GLenum->
                                                           GLenum
                                                           -> (Eff (webgl :: WebGl | eff) ret)

foreign import getShaderParameter_:: forall eff ret. WebGLShader->
                                                     GLenum
                                                     -> (Eff (webgl :: WebGl | eff) ret)

foreign import getShaderInfoLog_:: forall eff. WebGLShader
                                               -> (Eff (webgl :: WebGl | eff) String)

foreign import getShaderSource_:: forall eff. WebGLShader
                                              -> (Eff (webgl :: WebGl | eff) String)

foreign import getTexParameter_:: forall eff ret. GLenum->
                                                  GLenum
                                                  -> (Eff (webgl :: WebGl | eff) ret)

foreign import getUniform_:: forall eff ret. WebGLProgram->
                                             WebGLUniformLocation
                                             -> (Eff (webgl :: WebGl | eff) ret)

foreign import getUniformLocation_:: forall eff. WebGLProgram->
                                                 String
                                                 -> (Eff (webgl :: WebGl | eff) WebGLUniformLocation)

foreign import getVertexAttrib_:: forall eff ret. GLuint->
                                                  GLenum
                                                  -> (Eff (webgl :: WebGl | eff) ret)

foreign import getVertexAttribOffset_:: forall eff. GLuint->
                                                    GLenum
                                                    -> (Eff (webgl :: WebGl | eff) GLsizeiptr)

foreign import hint_:: forall eff. GLenum->
                                   GLenum
                                   -> (Eff (webgl :: WebGl | eff) Unit)

foreign import isBuffer_:: forall eff. WebGLBuffer
                                       -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import isEnabled_:: forall eff. GLenum
                                        -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import isFramebuffer_:: forall eff. WebGLFramebuffer
                                            -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import isProgram_:: forall eff. WebGLProgram
                                        -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import isRenderbuffer_:: forall eff. WebGLRenderbuffer
                                             -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import isShader_:: forall eff. WebGLShader
                                       -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import isTexture_:: forall eff. WebGLTexture
                                        -> (Eff (webgl :: WebGl | eff) GLboolean)

foreign import lineWidth_:: forall eff. GLfloat
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import linkProgram_:: forall eff. WebGLProgram
                                          -> (Eff (webgl :: WebGl | eff) Unit)

foreign import pixelStorei_:: forall eff. GLenum->
                                          GLint
                                          -> (Eff (webgl :: WebGl | eff) Unit)

foreign import polygonOffset_:: forall eff. GLfloat->
                                            GLfloat
                                            -> (Eff (webgl :: WebGl | eff) Unit)

foreign import readPixels_:: forall eff. GLint->
                                         GLint->
                                         GLsizei->
                                         GLsizei->
                                         GLenum->
                                         GLenum->
                                         ArrayBufferView
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import renderbufferStorage_:: forall eff. GLenum->
                                                  GLenum->
                                                  GLsizei->
                                                  GLsizei
                                                  -> (Eff (webgl :: WebGl | eff) Unit)

foreign import sampleCoverage_:: forall eff. GLclampf->
                                             GLboolean
                                             -> (Eff (webgl :: WebGl | eff) Unit)

foreign import scissor_:: forall eff. GLint->
                                      GLint->
                                      GLsizei->
                                      GLsizei
                                      -> (Eff (webgl :: WebGl | eff) Unit)

foreign import shaderSource_:: forall eff. WebGLShader->
                                           String
                                           -> (Eff (webgl :: WebGl | eff) Unit)

foreign import stencilFunc_:: forall eff. GLenum->
                                          GLint->
                                          GLuint
                                          -> (Eff (webgl :: WebGl | eff) Unit)

foreign import stencilFuncSeparate_:: forall eff. GLenum->
                                                  GLenum->
                                                  GLint->
                                                  GLuint
                                                  -> (Eff (webgl :: WebGl | eff) Unit)

foreign import stencilMask_:: forall eff. GLuint
                                          -> (Eff (webgl :: WebGl | eff) Unit)

foreign import stencilMaskSeparate_:: forall eff. GLenum->
                                                  GLuint
                                                  -> (Eff (webgl :: WebGl | eff) Unit)

foreign import stencilOp_:: forall eff. GLenum->
                                        GLenum->
                                        GLenum
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import stencilOpSeparate_:: forall eff. GLenum->
                                                GLenum->
                                                GLenum->
                                                GLenum
                                                -> (Eff (webgl :: WebGl | eff) Unit)

foreign import texImage2D_:: forall eff. GLenum->
                                         GLint->
                                         GLenum->
                                         GLsizei->
                                         GLsizei->
                                         GLint->
                                         GLenum->
                                         GLenum->
                                         ArrayBufferView
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import texParameterf_:: forall eff. GLenum->
                                            GLenum->
                                            GLfloat
                                            -> (Eff (webgl :: WebGl | eff) Unit)

foreign import texParameteri_:: forall eff. GLenum->
                                            GLenum->
                                            GLint
                                            -> (Eff (webgl :: WebGl | eff) Unit)

foreign import texSubImage2D_:: forall eff. GLenum->
                                            GLint->
                                            GLint->
                                            GLint->
                                            GLsizei->
                                            GLsizei->
                                            GLenum->
                                            GLenum->
                                            ArrayBufferView
                                            -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform1f_:: forall eff. WebGLUniformLocation->
                                        GLfloat
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform1fv_:: forall eff. WebGLUniformLocation->
                                         FloatArray
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform1i_:: forall eff. WebGLUniformLocation->
                                        GLint
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform1iv_:: forall eff. WebGLUniformLocation->
                                         Int32Array
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform2f_:: forall eff. WebGLUniformLocation->
                                        GLfloat->
                                        GLfloat
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform2fv_:: forall eff. WebGLUniformLocation->
                                         FloatArray
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform2i_:: forall eff. WebGLUniformLocation->
                                        GLint->
                                        GLint
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform2iv_:: forall eff. WebGLUniformLocation->
                                         Int32Array
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform3f_:: forall eff. WebGLUniformLocation->
                                        GLfloat->
                                        GLfloat->
                                        GLfloat
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform3fv_:: forall eff. WebGLUniformLocation->
                                         FloatArray
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform3i_:: forall eff. WebGLUniformLocation->
                                        GLint->
                                        GLint->
                                        GLint
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform3iv_:: forall eff. WebGLUniformLocation->
                                         Int32Array
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform4f_:: forall eff. WebGLUniformLocation->
                                        GLfloat->
                                        GLfloat->
                                        GLfloat->
                                        GLfloat
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform4fv_:: forall eff. WebGLUniformLocation->
                                         FloatArray
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform4i_:: forall eff. WebGLUniformLocation->
                                        GLint->
                                        GLint->
                                        GLint->
                                        GLint
                                        -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniform4iv_:: forall eff. WebGLUniformLocation->
                                         Int32Array
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniformMatrix2fv_:: forall eff. WebGLUniformLocation->
                                               GLboolean->
                                               FloatArray
                                               -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniformMatrix3fv_:: forall eff. WebGLUniformLocation->
                                               GLboolean->
                                               FloatArray
                                               -> (Eff (webgl :: WebGl | eff) Unit)

foreign import uniformMatrix4fv_:: forall eff. WebGLUniformLocation->
                                               GLboolean->
                                               FloatArray
                                               -> (Eff (webgl :: WebGl | eff) Unit)

foreign import useProgram_:: forall eff. WebGLProgram
                                         -> (Eff (webgl :: WebGl | eff) Unit)

foreign import validateProgram_:: forall eff. WebGLProgram
                                              -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib1f_:: forall eff. GLuint->
                                             GLfloat
                                             -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib1fv_:: forall eff. GLuint->
                                              FloatArray
                                              -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib2f_:: forall eff. GLuint->
                                             GLfloat->
                                             GLfloat
                                             -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib2fv_:: forall eff. GLuint->
                                              FloatArray
                                              -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib3f_:: forall eff. GLuint->
                                             GLfloat->
                                             GLfloat->
                                             GLfloat
                                             -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib3fv_:: forall eff. GLuint->
                                              FloatArray
                                              -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib4f_:: forall eff. GLuint->
                                             GLfloat->
                                             GLfloat->
                                             GLfloat->
                                             GLfloat
                                             -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttrib4fv_:: forall eff. GLuint->
                                              FloatArray
                                              -> (Eff (webgl :: WebGl | eff) Unit)

foreign import vertexAttribPointer_:: forall eff. GLuint->
                                                  GLint->
                                                  GLenum->
                                                  GLboolean->
                                                  GLsizei->
                                                  GLintptr
                                                  -> (Eff (webgl :: WebGl | eff) Unit)

foreign import viewport_:: forall eff. GLint->
                                       GLint->
                                       GLsizei->
                                       GLsizei
                                       -> (Eff (webgl :: WebGl | eff) Unit)
