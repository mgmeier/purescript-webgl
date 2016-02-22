## Module Control.Monad.Eff.WebGL

WebGL binding for purescript

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


