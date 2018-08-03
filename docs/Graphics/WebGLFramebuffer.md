## Module Graphics.WebGLFramebuffer

Framebuffers for the WebGL binding for purescript

#### `WebGLBuf`

```purescript
newtype WebGLBuf
  = WebGLBuf WebGLFramebuffer
```

#### `WebGLRendBuf`

```purescript
newtype WebGLRendBuf
  = WebGLRendBuf WebGLRenderbuffer
```

#### `RenderbufferFormat`

```purescript
data RenderbufferFormat
  = RGBA4
  | RGB565
  | RGB5_A1
  | DEPTH_COMPONENT16
```

#### `AttachementPoint`

```purescript
data AttachementPoint
  = COLOR_ATTACHMENT0
  | DEPTH_ATTACHMENT
  | STENCIL_ATTACHMENT
  | DEPTH_STENCIL_ATTACHMENT
```

#### `FrameBufferCode`

```purescript
data FrameBufferCode
  = FRAMEBUFFER_COMPLETE
  | FRAMEBUFFER_INCOMPLETE_ATTACHMENT
  | FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
  | FRAMEBUFFER_INCOMPLETE_DIMENSIONS
  | FRAMEBUFFER_UNSUPPORTED
```

#### `frameBufferCodeToConst`

```purescript
frameBufferCodeToConst :: FrameBufferCode -> GLenum
```

#### `checkFramebufferStatus`

```purescript
checkFramebufferStatus :: GLenum -> Effect GLenum
```

#### `createFramebuffer`

```purescript
createFramebuffer :: Effect WebGLBuf
```

#### `bindFramebuffer`

```purescript
bindFramebuffer :: WebGLBuf -> Effect Unit
```

#### `unbindFramebuffer`

```purescript
unbindFramebuffer :: Effect Unit
```

#### `createRenderbuffer`

```purescript
createRenderbuffer :: Effect WebGLRendBuf
```

#### `bindRenderbuffer`

```purescript
bindRenderbuffer :: WebGLRendBuf -> Effect Unit
```

#### `unbindRenderbuffer`

```purescript
unbindRenderbuffer :: Effect Unit
```

#### `renderbufferStorage`

```purescript
renderbufferStorage :: RenderbufferFormat -> Int -> Int -> Effect Unit
```

#### `framebufferRenderbuffer`

```purescript
framebufferRenderbuffer :: AttachementPoint -> WebGLRendBuf -> Effect Unit
```

#### `framebufferTexture2D`

```purescript
framebufferTexture2D :: AttachementPoint -> TargetType -> WebGLTex -> Effect Unit
```

#### `readPixels`

```purescript
readPixels :: GLint -> GLint -> GLsizei -> GLsizei -> Uint8Array -> Effect Uint8Array
```
