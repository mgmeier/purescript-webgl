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

#### `checkFramebufferStatus`

``` purescript
checkFramebufferStatus :: forall eff. GLenum -> Eff (webgl :: WebGl | eff) GLenum
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
renderbufferStorage :: forall eff. RenderbufferFormat -> Int -> Int -> EffWebGL eff Unit
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


