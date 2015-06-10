## Module Data.StrMap.ST.Unsafe

#### `unsafeGet`

``` purescript
unsafeGet :: forall a h r. STStrMap h a -> Eff (st :: ST h | r) (StrMap a)
```

Unsafely get the value for a key in a map.

This function does not check whether the key exists in the map.


