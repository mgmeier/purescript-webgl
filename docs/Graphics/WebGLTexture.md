## Module Graphics.WebGLTexture

Textures for the WebGL binding for purescript

#### `WebGLTex`

```purescript
newtype WebGLTex
  = WebGLTex WebGLTexture
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

#### `targetTypeToConst`

```purescript
targetTypeToConst :: TargetType -> GLenum
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

#### `SymbolicParameter`

```purescript
data SymbolicParameter
  = PACK_ALIGNMENT
  | UNPACK_ALIGNMENT
  | UNPACK_FLIP_Y_WEBGL
  | UNPACK_PREMULTIPLY_ALPHA_WEBGL
  | UNPACK_COLORSPACE_CONVERSION_WEBGL
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

#### `texture2DFor`

```purescript
texture2DFor :: forall a. String -> TexFilterSpec -> (WebGLTex -> Effect a) -> Effect Unit
```

#### `handleLoad2D`

```purescript
handleLoad2D :: forall a. WebGLTex -> TexFilterSpec -> a -> Effect Unit
```

#### `handleSubLoad2D`

```purescript
handleSubLoad2D :: forall a. WebGLTex -> Int -> Int -> Int -> Int -> TexFilterSpec -> a -> Effect Unit
```

#### `newTexture`

```purescript
newTexture :: Int -> Int -> TexFilterSpec -> Effect WebGLTex
```

#### `newTextureInit`

```purescript
newTextureInit :: Int -> Int -> TexFilterSpec -> Effect WebGLTex
```

#### `withTexture2D`

```purescript
withTexture2D :: forall typ. WebGLTex -> Int -> Uniform typ -> Int -> Effect Unit -> Effect Unit
```

#### `bindTexture`

```purescript
bindTexture :: TargetType -> WebGLTex -> Effect Unit
```

#### `unbindTexture`

```purescript
unbindTexture :: TargetType -> Effect Unit
```

#### `activeTexture`

```purescript
activeTexture :: Int -> Effect Unit
```

#### `createTexture`

```purescript
createTexture :: Effect WebGLTex
```
