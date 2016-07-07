## Module Graphics.WebGLTexture

Textures for the WebGL binding for purescript

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

#### `targetTypeToConst`

``` purescript
targetTypeToConst :: TargetType -> GLenum
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

#### `handleSubLoad2D`

``` purescript
handleSubLoad2D :: forall eff a. WebGLTex -> Int -> Int -> Int -> Int -> TexFilterSpec -> a -> EffWebGL eff Unit
```

#### `newTexture`

``` purescript
newTexture :: forall eff. Int -> Int -> TexFilterSpec -> EffWebGL eff WebGLTex
```

#### `newTextureInit`

``` purescript
newTextureInit :: forall eff. Int -> Int -> TexFilterSpec -> EffWebGL eff WebGLTex
```

#### `withTexture2D`

``` purescript
withTexture2D :: forall eff typ. WebGLTex -> Int -> Uniform typ -> Int -> EffWebGL eff Unit -> EffWebGL eff Unit
```

#### `bindTexture`

``` purescript
bindTexture :: forall eff. TargetType -> WebGLTex -> EffWebGL eff Unit
```

#### `unbindTexture`

``` purescript
unbindTexture :: forall eff. TargetType -> EffWebGL eff Unit
```

#### `activeTexture`

``` purescript
activeTexture :: forall eff. Int -> Eff (webgl :: WebGl | eff) Unit
```

#### `createTexture`

``` purescript
createTexture :: forall eff. Eff (webgl :: WebGl | eff) WebGLTex
```


