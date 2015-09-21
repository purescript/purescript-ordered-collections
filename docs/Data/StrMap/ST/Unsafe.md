## Module Data.StrMap.ST.Unsafe

#### `unsafeGet`

``` purescript
unsafeGet :: forall a h r. STStrMap h a -> Eff (st :: ST h | r) (StrMap a)
```

Unsafely get the map out of ST without copying it

If you later change the ST version of the map the pure value will also change.


